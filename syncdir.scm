#!/usr/bin/guile \
-e main -s
!#


(use-modules (srfi srfi-1)
             (srfi srfi-2)
             (srfi srfi-11)
             (srfi srfi-26)
             (ice-9 popen)
             (ice-9 textual-ports)
             (ice-9 pretty-print)
             (ice-9 format)
             (ice-9 ftw))


(define +program-name+                  "syncdir.scm")
(define +config-file-basename+          "syncdir.scm")
(define +saved-times-file-basename+     ".sync-times")
(define +default-editor+                "vi")

(define +unix-time-comparison-thresh+   2)
(define +cmd-var-char-set+
  (char-set-union
   (char-set-intersection
    char-set:ascii
    char-set:letter+digit)
   (char-set #\_)))

(define +shell-escaping-rules+
  `((bourne-shell-single-quotes
     ,char-set:empty
     (#\' . "'\"'\"'"))
    (bourne-shell-double-quotes
     ,(char-set #\$ #\` #\\ #\") (#\! . "\"'!'\""))
    (bourne-shell
     ,(string->char-set "><;&|#$!*?[ {}`\\\"'"))))
(define +default-shell-escaping-rule-string+
  'bourne-shell-double-quotes)
(define +default-shell-escaping-rule-list+
  'bourne-shell)


(define *config-alist* (make-parameter '()))
(define *ignore-globs* (make-parameter '()))
(define *verbose* (make-parameter #f))


(define (glob-match glob s)
  (define bracket-expression-complement-char #\!)
  (define bracket-expression-range-char #\-)
  (define (match-char ch ndrop)
    (and (not (string-null? s))
         (char=? ch (string-ref s 0))
         (glob-match (string-drop glob ndrop) (string-drop s 1))))
  (define* (skip-to ch #:optional (from (string-drop glob 1)))
    (let skip-more ((g from))
      (and (not (string-null? g))
           (case (string-ref g 0)
             ((#\\) (and (>= (string-length g) 2)
                         (skip-more (string-drop g 2))))
             ((#\[) (and=> (string-index g #\] 2)
                           (lambda (end-idx)
                             (skip-more (string-drop g (1+ end-idx))))))
             (else => (lambda (this-ch)
                        (let ((drop-this (string-drop g 1)))
                          (cond
                           ((char=? this-ch #\{)
                            (and=> (skip-to #\} drop-this) skip-more))
                           ((char=? this-ch ch)
                            drop-this)
                           ((char=? this-ch #\})
                            #f)
                           (else
                            (skip-more drop-this))))))))))
  (define (bracket-expression->char-set s)
    (let ((s (string-drop (string-drop-right s 1) 1)))
      (if (= (string-length s) 1)
          (values (string->char-set s) 1)
          (let-values (((s result)
                        (if (char=? (string-ref s 0)
                                    bracket-expression-complement-char)
                            (values (string-drop s 1) char-set-complement)
                            (values s identity))))
            (let parse-more ((cs (char-set)) (s s) (prev #f))
              (if (string-null? s)
                  (result cs)
                  (let ((c (string-ref s 0)))
                    (if (and prev
                             (char=? c bracket-expression-range-char)
                             (>= (string-length s) 2))
                        (parse-more
                         (char-set-union cs (ucs-range->char-set
                                             (char->integer prev)
                                             (1+ (char->integer (string-ref s 1)))))
                         (string-drop s 2)
                         #f)
                        (parse-more
                         (char-set-adjoin cs c)
                         (string-drop s 1)
                         c)))))))))
  (if (string-null? glob)
      (string-null? s)
      (case (string-ref glob 0)
        ((#\?) (and (not (string-null? s))
                      (glob-match (string-drop glob 1) (string-drop s 1))))
        ((#\*) (or (glob-match (string-drop glob 1) s)
                     (and (not (string-null? s))
                        (glob-match glob (string-drop s 1)))))
        ((#\[) (and=> (string-index glob #\] 2)
                        (lambda (end-idx)
                          (not (string-null? s))
                          (char-set-contains?
                           (bracket-expression->char-set
                          (string-take glob (1+ end-idx)))
                           (string-ref s 0))
                          (glob-match (string-drop glob (1+ end-idx))
                                    (string-drop s 1)))))
        ((#\\) (and (>= (string-length glob) 2)
                      (match-char (string-ref glob 1) 2)))
        ((#\, #\}) s)
        ((#\{)
         (let next-branch ((g (string-drop glob 1)))
           (or (and=> (glob-match g s)
                      (lambda (s)
                        (and (string? s)
                             (glob-match (skip-to #\}) s))))
                 (and=> (skip-to #\, g) next-branch))))
        (else => (cut match-char <> 1)))))

(define (call-with-port port proc)
  (let ((res (proc port)))
    (close-port port)
    res))

(define (unix-time~=? a b)
  (<= (abs (- a b)) +unix-time-comparison-thresh+))


;;; Paths

(define (path-join x . l)
  (string-join (cons x l) file-name-separator-string))

(define (path-local? path)              ; -> <bool>
  (not (and-let* ((i (string-index path #\:))
                  ((not (string-index path #\/ 0 i)))))))

(define (relative-path path)
  (let ((cwd (getcwd)))
    (if (string-prefix? cwd path)
        (string-copy path (+ (string-length cwd)
                             (if (file-name-separator?
                                  (string-ref
                                   cwd (1- (string-length cwd))))
                                 0 1)))
        path)))

(define (realpath path)
  (if (eq? (stat:type (lstat path)) 'symlink)
      (realpath (readlink path))
      path))

(define (expand-user-dir path)
  (if (or (string-null? path)
          (not (char=? (string-ref path 0) #\~)))
      path
      (let* ((prefix-len (string-skip path (negate file-name-separator?)))
             (username (string-drop (string-take path prefix-len) 1))
             (suffix (string-drop path prefix-len))
             (dir (and=> (false-if-exception
                          (if (string-null? username)
                              (getpwuid (getuid))
                              (getpwnam username)))
                         passwd:dir)))
        (if dir (string-append dir suffix) path))))


;;; Shell command escaping

(define (default-escaping template)
  (cond
   ((list? template) +default-shell-escaping-rule-list+)
   ((string? template) +default-shell-escaping-rule-string+)
   (else #f)))

(define (string-escape s escape-char-set special)
  (string-fold
   (lambda (c s)
     (string-append s (cond
                       ((char-set-contains? escape-char-set c)
                        (string #\\ c))
                       ((assq c special) => cdr)
                       ((or (char-set-contains? char-set:graphic c)
                            (char-set-contains? char-set:blank c))
                        (string c))
                       (else
                        (format #f "\\x~x" (char->integer c))))))
   "" s))

(define (string-escape-for-rules s r)
  (define (symbol->escaping sym)
    (or (assq-ref +shell-escaping-rules+ sym)
        (error "Unknown escaping rule" sym)))
  (let-values (((base r)
                (if (and (pair? r)
                         (symbol? (car r)))
                    (values (symbol->escaping (car r))
                            (cdr r))
                    (values (cons char-set:empty '())
                            r))))
    (let ((other
           (cond
            ((not r)
             (list char-set:empty))
            ((symbol? r)
             (symbol->escaping r))
            ((list? r) (cons (string->char-set (car r)) (cdr r)))
            (else
             (error
              "Escaping rules should be #f, symbol or list, not" r)))))
      (string-escape
       s
       (char-set-union (car base) (car other))
       (append (cdr base) (cdr other))))))


;;; Command templates

(define (string->cmd-template s)
  (let ((t (if (or (string-null? s)
                   (not (char=? (string-ref s 0) #\()))
               s
               (call-with-input-string s read))))
    (cons (default-escaping t) t)))

(define (expand-cmd-list escaping template replace-alist)
  (string-join
   (map (lambda (x)
          (if (string? x)
              x
              (string-escape-for-rules
               (assq-ref replace-alist x)
               escaping)))
        template)
   " "))

(define (expand-cmd-string escaping template replace-alist)
  (define (expand buf)
    (string-escape-for-rules
     (or (assq-ref replace-alist (list->symbol (reverse buf)))
         (error "Invalid substitution in command" template))
     escaping))
  (define (identifier-char? c)
    (char-set-contains? +cmd-var-char-set+ c))
  (define (process-char-nobuf res c)
    (if (char=? c #\$)
        (list res '() #f)
        (list (string-append/shared res (string c))
              #f #f)))
  (call-with-values
      (lambda ()
        (apply
         values
         (string-fold
          (lambda (c s)
            (let-values (((res buf braced?) (apply values s)))
              (cond
               ((not buf)
                (process-char-nobuf res c))
               ((and braced? (char=? c #\}))
                (list (string-append res (expand buf)) #f #f))
               ((and (null? buf) (not braced?) (char=? c #\{))
                (list res '() #t))
               ((and (not braced?) (not (identifier-char? c)))
                (process-char-nobuf (string-append res (expand buf)) c))
               (else
                (list res (cons c buf) braced?)))))
          '("" #f #f)
          template)))
    (lambda (res buf braced?)
      (if (and buf braced?)
          (error "Incomplete substitution in command" template))
      (if buf
          (string-append res (expand buf))
          res))))

(define (string-1-list->string-maybe t)
  (if (and (pair? t)
           (string? (car t))
           (null? (cdr t)))
      (car t)
      t))

(define (escaping-and-template cmd)
  (let-values (((e t)
                (if (or (string? cmd)
                        (and (pair? cmd)
                             (string? (car cmd))))
                    (values 'default cmd)
                    (car+cdr cmd))))
    (values e (string-1-list->string-maybe t))))

(define (expand-cmd cmd replace-alist)
  (let-values (((escaping template) (escaping-and-template cmd)))
    ((cond
      ((list? template) expand-cmd-list)
      ((string? template) expand-cmd-string)
      (else
       (error "Expected command to be a string or list, got" template)))
     (if (eq? escaping 'default)
         (default-escaping template)
         escaping)
     template
     replace-alist)))

(define (expand-merge-cmd merge-cmd a b o)
  (expand-cmd merge-cmd
              `((a . ,a)
                (b . ,b)
                (output . ,o))))

(define (editor->merge-cmd editor)
  (list editor 'a 'b 'output))


;;; Config file

(define (config-path)
  (string-append
   (or (getenv "XDG_CONFIG_PATH")
       (string-append
        (or (getenv "HOME")
            (passwd:dir (getpwuid (getuid))))
        file-name-separator-string
        ".config"
        file-name-separator-string))
   +config-file-basename+))

(define (read-config)
  (or (false-if-exception
       (call-with-input-file
           (config-path)
         read))
      '()))


;;; Saved times file

(define (saved-times-file-name local-path)
  (path-join local-path +saved-times-file-basename+))

(define (read-saved-times local-path)
  (or (false-if-exception
       (call-with-input-file
           (saved-times-file-name local-path)
         read))
      '()))

(define (write-saved-times times local-path)
  (call-with-output-file
      (saved-times-file-name local-path)
    (cut format <> "~s~%" times)))


;;; Modification times

(define (parse-mtime datetime-string)        ; -> <number>, unix time
  (car (mktime (car (strptime "%Y-%m-%d %H:%M:%S" datetime-string)))))


;;; rclone

(define (rclone-file-times rclone-path)
  (call-with-port
   (open-pipe*
    OPEN_READ
    "rclone" "lsf" "-R" "--files-only" "--format" "tp" rclone-path)
   (lambda (pp)
     (let loop ((lst '()))
       (let ((l (get-line pp)))
         (if (eof-object? l) lst
             (let* ((idx (string-index l #\;))
                    (t (parse-mtime (string-take l idx))))
               (loop (acons (string-drop l (1+ idx)) t lst)))))))))

(define (rclone-copy-file src-rclone-path dest-rclone-path)
  (system* "rclone" "copyto" src-rclone-path dest-rclone-path))


;;; Local files

(define local-mtime
  (compose stat:mtime stat))

(define (local-file-list local-path)
  (define (nth x)
    (lambda a (list-ref a x)))
  (file-system-fold
   (const #t)
   (lambda (p s r) (cons p r))
   (nth 2)
   (nth 2)
   (nth 2)
   (nth 3)
   '()
   local-path))

(define (local-file-times local-path)
  (filter-map
   (lambda (fn)
     (and=> (false-if-exception (local-mtime fn))
            (cut cons
                 (string-trim-both
                  (string-drop fn (string-length local-path))
                  file-name-separator?)
                 <>)))
   (local-file-list local-path)))


;;; Rules

;;; -> a->b | b->a | both | #f
(define (sync-direction saved-time a-time b-time)
  (cond
   ((not a-time) 'b->a)
   ((not b-time) 'a->b)
   (else
    (if (or (not saved-time)
            (unix-time~=? a-time saved-time)
            (unix-time~=? b-time saved-time))
        (cond
         ((unix-time~=? a-time b-time) #f)
         ((> a-time b-time) 'a->b)
         (else 'b->a))
        'both))))

(define* (all-files a-tab b-tab)
  (filter (lambda (fn)
            (not (any (cute glob-match <> (basename fn))
                      (*ignore-globs*))))
          (delete
           +saved-times-file-basename+
           (lset-union string=?
                       (map car a-tab)
                       (map car b-tab)))))


;;; Operations

(define (file-times path)
  ((if (path-local? path)
       local-file-times
       rclone-file-times)
   path))

(define (copy a b)
  (when (*verbose*)
    (format #t "copy ~s ~s~%" a b))
  (rclone-copy-file a b))

(define silent-copy rclone-copy-file)

(define (merge a-orig b-orig)
  (when (*verbose*)
    (format #t "merge ~s ~s... " a-orig b-orig))
  (let ((a (tmpnam))
        (b (tmpnam))
        (o (tmpnam))
        (merge-cmd (or (and-let*
                        ((e (getenv "SYNCDIR_MERGE"))
                         ((not (string-null? e))))
                        (string->cmd-template e))
                       (assq-ref (*config-alist*) 'merge-cmd)
                       (editor->merge-cmd
                        (or (getenv "EDITOR")
                            (getenv "VISUAL")
                            +default-editor+)))))
    (silent-copy a-orig a)
    (silent-copy b-orig b)
    (let* ((real-merge-cmd ((lambda (cmd)
                              (format (current-error-port)
                                      "command: ~s~%"
                                      cmd)
                              cmd)
                            (expand-merge-cmd merge-cmd a b o)))
           (st (and (and=>
                     (status:exit-val
                      (system real-merge-cmd)) zero?)
                    (access? o R_OK))))
      (delete-file a)
      (delete-file b)
      (if st
          (begin
            (when (*verbose*)
              (display "done")
              (newline))
            o)
          (begin
            (false-if-exception
             (delete-file o))
            (when (*verbose*)
              (display "cancelled")
              (newline))
            #f)))))


;;; if direction not #f, sync and record time
;;; else, copy both as tmp files and run merge
;;; if merge successful, save this one to both places
;;; else, skip this file
(define (process-file saved-tab a-tab b-tab a-place b-place fname)
  (let ((s (assoc-ref saved-tab fname))
        (a (assoc-ref a-tab fname))
        (b (assoc-ref b-tab fname))
        (af (path-join a-place fname))
        (bf (path-join b-place fname)))
    (case (sync-direction s a b)
      ((a->b)
       (copy af bf)
       a)
      ((b->a)
       (copy bf af)
       b)
      ((both)
       (let ((out-fname (merge af bf)))
         (if out-fname
             (begin
               (silent-copy out-fname af)
               (silent-copy out-fname bf)
               (let ((mtime (stat:mtime (stat out-fname))))
                 (delete-file out-fname)
                 mtime))
             s)))
      (else s))))

(define (die fmt . args)
  (format (current-error-port)
          "~a: ~k~%"
          +program-name+
          fmt args)
  (exit 1))

(define (run-sync . paths)
  (let-values (((local remote) (partition path-local? paths)))
    (if (or (null? local) (null? remote))
        (die "error: there must be one local and one remote path")
        (let* ((local-path
                (relative-path
                 (realpath
                  (expand-user-dir (car local)))))
               (a local-path)
               (b (car remote))
               (at (local-file-times a))
               (bt (rclone-file-times b))
               (st (read-saved-times local-path))
               (new-saved-tab
                (let loop ((new-tab '())
                           (files (all-files at bt)))
                  (if (null? files) new-tab
                      (let* ((f (car files))
                             (new-times
                              (process-file
                               st at bt a b f)))
                        (loop (acons f new-times new-tab)
                              (cdr files)))))))
          (write-saved-times new-saved-tab local-path)))))

(define (main argv)
  (let ((len (length argv))
        (config (read-config)))
    (parameterize
        ((*config-alist* config)
         (*verbose* #t)                 ; keep old behavior
         (*ignore-globs*
          (assq-ref config 'ignore-globs)))
      (cond
       ((= len 3)
        (apply run-sync (cdr argv)))
       ((= len 2)
        (let* ((path-id (string->symbol (second argv)))
               (entry
                (and=> (assq-ref config 'paths)
                       (cut assq path-id <>))))
          (if entry (apply run-sync (cdr entry))
              (die "error: preconfigured path ~a not found"
                   path-id))))
       (else
        (format #t "usage:~%  ~a path-a path-b~%  ~a preconf-name~%"
                +program-name+ +program-name+))))))
