#!/usr/bin/guile \
-e main -s
!#


(use-modules (srfi srfi-1)
             (srfi srfi-2)
             (srfi srfi-11)
             (srfi srfi-26)
             (srfi srfi-37)
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


(define *ignore-globs* (make-parameter '()))
(define *verbosity* (make-parameter #f))
(define *merge-command* (make-parameter #f))
(define *dummy?* (make-parameter #f))


;;; note: this one returns #f if ‘cond’ as false and ‘else’ clause is
;;; missing
(define-syntax aif
  (lambda (x)
    (syntax-case x ()
      ((_ cond then rest ...)
       (let ((it (datum->syntax x 'it)))
         #`(let ((#,it cond))
             #,(syntax-case #'(then rest ...) ()
                 ((then) #`(and #,it then))
                 ((then else) #`(if #,it then else)))))))))


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
             ((#\[) (aif (string-index g #\] 2)
                         (skip-more (string-drop g (1+ it)))))
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
                            (values (string-drop s 1)
                                    char-set-complement)
                            (values s identity))))
            (let parse-more ((cs (char-set)) (s s) (prev #f))
              (if (string-null? s)
                  (result cs)
                  (let ((c (string-ref s 0)))
                    (if (and prev
                             (char=? c bracket-expression-range-char)
                             (>= (string-length s) 2))
                        (parse-more
                         (char-set-union
                          cs
                          (ucs-range->char-set
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
                    (glob-match (string-drop glob 1)
                                (string-drop s 1))))
        ((#\*) (or (glob-match (string-drop glob 1) s)
                   (and (not (string-null? s))
                        (glob-match glob (string-drop s 1)))))
        ((#\[) (aif (string-index glob #\] 2)
                    (and (not (string-null? s))
                         (char-set-contains?
                          (bracket-expression->char-set
                           (string-take glob (1+ it)))
                          (string-ref s 0))
                         (glob-match (string-drop glob (1+ it))
                                     (string-drop s 1)))))
        ((#\\) (and (>= (string-length glob) 2)
                    (match-char (string-ref glob 1) 2)))
        ((#\, #\}) s)
        ((#\{)
         (let next-branch ((g (string-drop glob 1)))
           (or (aif (glob-match g s)
                    (and (string? it)
                         (glob-match (skip-to #\}) it)))
               (and=> (skip-to #\, g) next-branch))))
        (else => (cut match-char <> 1)))))

(define (unix-time~=? a b)
  (<= (abs (- a b)) +unix-time-comparison-thresh+))

(define (getenv-non-null e)
  (and-let* ((v (getenv e))
             ((not (string-null? v))))
            v))

(define (verbose? x)
  (if (number? x)
      (>= (*verbosity*) x)
      (verbose? (assq-ref
                 '((verbose . 1)
                   (normal . 0)
                   (quiet . -1)
                   (silent . -2))
                 x))))

(define (exit-status-ok? es)
  (zero? (status:exit-val es)))


;;;; Error handling

(define (throw* key msg . fmt)
  (throw key #f msg fmt #f))

(define (display-exception port key . args)
  (format port "Throw to key `~a'" key)
  (aif (false-if-exception
        (call-with-output-string
         (lambda (s)
           (apply display-error #f s args))))
       (format port ":~%~a" it)
       (format port " with args `~s'~%" args)))

(define-syntax catch-exceptions
  (lambda (x)
    (syntax-case x ()
      ((_ expr error-expr)

       #'(let ((stack #f))
           (catch
            #t
            (lambda ()
              expr)
            (lambda (k . args)
              (when (verbose? 'verbose)
                (display-backtrace
                 stack
                 (current-error-port)))
              (apply display-exception
                     (current-error-port) k args)
              error-expr)
            (lambda (k . args)
              (set! stack (make-stack #t 2)))))))))

;;; not used yet
(define-syntax catch-case
  (lambda (x)
    (syntax-case x ()
      ((_ expr spec spec* ...)
       (let ((key (datum->syntax x 'key))
             (args (datum->syntax x 'args)))
         #`(catch #t
                  (lambda () expr)
                  (lambda (key . args)
                    (let ((#,key key)
                          (#,args args))
                      (case key
                        spec spec* ...)))))))))


;;;; Paths

(define (path-join x . l)
  (string-join (map (cut string-trim-right <> file-name-separator?)
                    (cons x l))
               file-name-separator-string))

(define (path-local? path)              ; -> <bool>
  (not (and-let* ((i (string-index path #\:))
                  ((not (string-index path #\/ 0 i)))))))

(define (both-local? . paths)
  (every path-local? paths))

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
      (let* ((prefix-len
              (string-skip path (negate file-name-separator?)))
             (username (string-drop (string-take path prefix-len) 1))
             (suffix (string-drop path prefix-len))
             (dir (and=> (false-if-exception
                          (if (string-null? username)
                              (getpwuid (getuid))
                              (getpwnam username)))
                         passwd:dir)))
        (if dir (string-append dir suffix) path))))


;;;; Commands

(define (substitute-list l s)
  (map (lambda (x)
         (if (symbol? x) (assq x s) x))
       l))

(define (expand-merge-cmd merge-cmd a b o)
  (substitute-list merge-cmd
                   `((a . ,a)
                     (b . ,b)
                     (output . ,o))))

(define (string-split-no-empty s pred)
  (filter (negate string-null?) (string-split s pred)))

(define (string->merge-cmd s)
  `(,@(string-split-no-empty s char-set:blank) 'a 'b 'output))

(define (open-output-pipe* name . args)
  (apply open-pipe* OPEN_WRITE name args))


;;;; Config file

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


;;;; Saved times file

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


;;;; Modification times

(define (parse-mtime datetime-string)        ; -> <number>, unix time
  (car (mktime (car (strptime "%Y-%m-%d %H:%M:%S" datetime-string)))))


;;;; rclone

(define (rclone-error subcommand)
  (throw 'rclone #f
         "`rclone ~A' returned non-zero" (list subcommand)
         #f))

(define (rclone-file-times rclone-path)
  (let* ((pp
          (open-pipe*
           OPEN_READ
           "rclone" "lsf" "-R" "--files-only"
           "--format" "tp" rclone-path))
         (times
          (let loop ((lst '()))
            (let ((l (get-line pp)))
              (if (eof-object? l) lst
                  (let* ((idx (string-index l #\;))
                         (t (parse-mtime (string-take l idx))))
                    (loop
                     (acons (string-drop l (1+ idx)) t lst))))))))
    (unless (exit-status-ok? (close-pipe pp))
      (rclone-error "lsf"))
    times))

(define (rclone-copy-file src dest)
  (unless (exit-status-ok? (system* "rclone" "copyto" src dest))
    (rclone-error "copyto")))

(define (rclone-copy-file-list src dest names)
  (let ((port (open-output-pipe*
               "rclone" "copy" "--files-from" "/dev/stdin" src dest)))
    ;; prepend slash to prevent names being interpreted as comments
    (for-each (cut format port "/~a~%" <>) names)
    (unless (exit-status-ok? (close-pipe port))
      (rclone-error "copy"))))


;;;; Local files

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
     (aif (false-if-exception (local-mtime fn))
          (cons
           (string-trim-both
            (string-drop fn (string-length local-path))
            file-name-separator?)
           it)))
   (local-file-list local-path)))


;;;; Rules

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


;;;; Operations

(define* (copy-one-file src dest #:optional (name #f))
  (define (make-filename dir-or-path)
    (if name (path-join dir-or-path name) dir-or-path))
  ((if (both-local? src dest)
       copy-file
       rclone-copy-file)
   (make-filename src)
   (make-filename dest)))

(define (copy-files src dest names)
  (if (both-local? src dest)
      (for-each (lambda (n)
                  (copy-file (path-join src n)
                             (path-join dest n)))
                names)
      (rclone-copy-file-list src dest names)))

;;; -> mtimes
(define (do-copies tab names src dest)
  (for-each
   (lambda (n)
     (format #t "copy~{ ~s~}~%"
             (map (cut path-join <> n)
                  (list src dest))))
   names)
  (unless (null? names)
    (copy-files src dest names))
  (map (lambda (n)
         (cons n (assoc-ref tab n)))
       names))

(define (merge mergedir paths name)
  (let* ((name-hash (hash name #x10000))
         (name-base (basename name))
         (merge-filenames
          (map (lambda (letter)
                 (format #f "~4,'0x-~a-~a"
                         name-hash
                         letter
                         name-base))
               '(#\a #\b #\o)))
         (input-filenames
          (take merge-filenames 2)))
    (when (verbose? 'quiet)
      (format #t "merge~{ ~s~}..." input-filenames))
    (for-each
     (cut copy-one-file <> <>)
     (map (cut path-join <> name) paths)
     (map (cut path-join mergedir <>) input-filenames))
    (let* ((full-merge-filenames
            (map (cut path-join mergedir <>)
                 merge-filenames))
           (output-filename
            (list-ref full-merge-filenames 2))
           (real-merge-cmd
            (apply expand-merge-cmd (cdr (*merge-command*))
                   full-merge-filenames))
           (st (and (exit-status-ok?
                     (apply system* real-merge-cmd))
                    (access? output-filename R_OK))))
      (for-each delete-file (take full-merge-filenames 2))
      (if st
          (begin
            (when (verbose? 'quiet)
              (display "done")
              (newline))
            output-filename)
          (begin
            (false-if-exception
             (delete-file output-filename))
            (when (verbose? 'quiet)
              (display "cancelled")
              (newline))
            #f)))))

;;; -> mtimes
(define (handle-merge mergedir saved-tab paths name)
  (let ((out-fname
         (false-if-exception
          (merge mergedir paths name))))
    (if out-fname
        (begin
          (for-each
           (cut copy-one-file out-fname <>)
           (map (cut path-join <> name) paths))
          (let ((mtime (stat:mtime (stat out-fname))))
            (delete-file out-fname)
            mtime))
        (assoc-ref saved-tab name))))

(define (get-actions a-tab b-tab saved-tab names)
  (map
   (lambda (n)
     (let ((s (assoc-ref saved-tab n))
           (a (assoc-ref a-tab n))
           (b (assoc-ref b-tab n)))
       (cons n (sync-direction s a b))))
   names))

(define (split-actions actions)
  (define (join-to-key x k l)
    (map
     (lambda (ll)
       (let-values (((key items) (car+cdr ll)))
         (if (eq? k key)
             (cons* key x items)
             ll)))
     l))
  (fold
   (lambda (a s)
     (join-to-key (car a) (cdr a) s))
   '((#f) (a->b) (b->a) (both))              ; (make-list 3 '())
   actions))

(define (delete-dir dir)
  (let more ((ds (opendir dir)))
    (let ((e (readdir ds)))
      (unless (eof-object? e)
        (if (member e '("." ".."))
            (more ds)
            (let ((fn (path-join dir e)))
              ((if (eq? (stat:type (stat fn)) 'directory)
                   delete-dir
                   delete-file)
               fn)))))))

(define (run-actions action names paths a-tab b-tab saved-tab)
  (case action
    ((#f)
     (map (lambda (n)
            (cons n (or (assoc-ref saved-tab n)
                        (assoc-ref a-tab n)
                        ;; third should never be reached
                        (assoc-ref b-tab n))))
          names))
    ((a->b)
     (apply do-copies a-tab names paths))
    ((b->a)
     (apply do-copies b-tab names (reverse paths)))
    ((both)
     (let ((mergedir
            (path-join "/tmp" (format #f "syncdir-merge-~a" (getpid)))))
       (mkdir mergedir)
       (let ((mtimes
              (filter-map
               (lambda (n)
                 (cons n
                       (handle-merge
                        mergedir
                        saved-tab
                        paths
                        n)))
               names)))
         (delete-dir mergedir)
         mtimes)))))

(define (dummy-display actions paths)
  (for-each
   (lambda (action)
     (let-values (((n a) (car+cdr action)))
       (when a
         (format #t "(dummy) ~a~{ ~s~}~%"
                 (if (eq? a 'both) 'merge 'copy)
                 ((if (eq? a 'b->a) reverse identity)
                  (map (cut path-join <> n) paths))))))
   actions))

(define (make-new-tab actions paths a-tab b-tab saved-tab)
  (concatenate
   (map
    (lambda (x)
      (run-actions (car x) (cdr x)
                   paths a-tab b-tab saved-tab))
    (split-actions actions))))

(define (run-sync . paths)
  (let-values (((local remote) (partition path-local? paths)))
    (if (or (null? local) (null? remote))
        (apply throw* 'bad-paths
               "expected one local and one remote path; got ~A and ~A"
               paths)
        (let* ((local-path
                (relative-path
                 (realpath
                  (expand-user-dir (car local)))))
               (a local-path)
               (b (car remote))
               (ab (list a b))
               (at (local-file-times a))
               (bt (rclone-file-times b))
               (st (read-saved-times local-path))
               (actions
                (get-actions at bt st (all-files at bt))))
          (if (*dummy?*)
              (dummy-display actions ab)
              (write-saved-times
               (make-new-tab
                actions ab at bt st)
               local-path))))))

(define (run-path-entry entry)
  (let ((cfg (cdddr entry)))
    (parameterize
        ((*ignore-globs*
          (append (*ignore-globs*)
                  (or (assq-ref cfg 'ignore-globs) '())))
         (*merge-command*
          (let ((old (*merge-command*)))
            (cond
             ((eq? (car old) 'env) old)
             ((assq-ref cfg 'merge-cmd)
              => (cut cons 'override <>))
             (else old)))))
      (apply run-sync (take (cdr entry) 2)))))

(define (run-entry entry paths)
  (or (and=> (assq entry paths) run-path-entry)
      (throw* 'bad-paths
              "named path ~A not found" entry)))


;;;; Command line

(define (display-usage)
  (format #t "usage: ~a [OPTIONS | NAMES]...~%"
          +program-name+))

;;; Guile issues a warning here, but it shouldn’t
(define (display-help-message)
  (format #t "Usage: ~a [OPTION | NAME]...~%~
              Synchronize and merge directories with remotes.~%~%~
              NAMEs are named path pair names, which are defined in~%~
              the config file.~%~%~
              ~:{~t -~a~{, --~a~} ~30t~a~%~}~%"
          +program-name+
          '((#\h ("help") "Display this message and exit")
            (#\v ("verbose") "Display exception backtraces")
            (#\q ("quiet") "Don't announce copies")
            (#\Q ("silent" "very-quiet")
             "Don't print anything to stdout")
            (#\n ("dummy" "dry-run")
             "Dummy mode: don't actually copy or merge anything")))
  (exit))

(define (parse-cmdline argv)
  (define (paths-error)
    (throw* 'bad-cmdline
            "expecting 2 arguments after `-p'/`--paths' option"))
  (define initial-seed
    '(()  #| entries |#
      0   #| verbosity |#
      #f  #| dummy mode |#
      ;; TODO:
      ;; #f  #| disable-default-config|#
      ;; '() #| additional-config-files |#
      ))

  (define (mutate-state-proc which how)
    (lambda (opt name arg seed)
      (let*-values (((hd tl*) (split-at seed which))
                    ((elt tl) (car+cdr tl*)))
        (append hd (list (how arg elt)) tl))))
  (define arg (lambda (arg opt) arg))
  (define (op proc . args)
    (lambda (opt name arg seed)
      (apply proc args) seed))
  (define (named-path name seed)
    (cons (cons (string->symbol name)
                (car seed))
          (cdr seed)))
  (define (bad-option-maybe opt name arg seed)
    (throw* 'bad-cmdline "unrecognized option: ~S" name))
  (apply
   values
   (args-fold
    (cdr argv)
    (list (option '(#\h "help") #f #f
                  (op display-help-message))
          (option '(#\v "verbose") #f #f
                  (mutate-state-proc 1 (const 1)))
          (option '(#\q "quiet") #f #f
                  (mutate-state-proc 1 (const -1)))
          (option '(#\Q "silent" "very-quiet") #f #f
                  (mutate-state-proc 1 (const -2)))
          (option '(#\n "dummy" "dry-run") #f #f
                  (mutate-state-proc 2 (const #t))))
    bad-option-maybe
    named-path
    initial-seed)))


;;;; Main

(define (config-global-options defaults config)
  (apply
   values
   (map (lambda (k)
          (or (and=> (assq k config) cdr)
              (assq-ref defaults k)))
        (map car defaults))))

(define (exit* v)
  (exit (if v EXIT_SUCCESS EXIT_FAILURE)))

(define (main argv)
  (let-values (((entries verbosity dummy?)
                (parse-cmdline argv)))
    (parameterize
        ((*verbosity* verbosity)
         (*dummy?* dummy?))
      (let ((config (read-config)))
        (let-values (((ignore-globs merge-cmd paths)
                      (config-global-options
                       '((ignore-globs)
                         (merge-cmd . #f)
                         (paths))
                       config)))
          (parameterize
              ((*ignore-globs* ignore-globs)
               (*merge-command*
                (or (aif (getenv-non-null "SYNCDIR_MERGE")
                         (cons 'env (string->merge-cmd it)))
                    (aif merge-cmd (cons 'config it))
                    (cons
                     'editor
                     (string->merge-cmd
                      (or (getenv-non-null "EDITOR")
                          (getenv-non-null "VISUAL")
                          (+default-editor+)))))))
            (if (null? entries)
                (display-usage)
                (exit*
                 (let process ((entries entries)
                               (status #t))
                   (if (null? entries)
                       status
                       (let-values (((entry rest) (car+cdr entries)))
                         (catch-exceptions
                          (begin
                            (run-entry entry paths)
                            (process rest status))
                          (process rest #f)))))))))))))
