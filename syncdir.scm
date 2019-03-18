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
             (ice-9 regex)
             (ice-9 ftw))


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

(define (parse-mtime datetime-string)        ; -> <number>, unix time
  (car (mktime (car (strptime "%Y-%m-%d %H:%M:%S" datetime-string)))))

(define local-mtime
  (compose stat:mtime stat))

(define (path-local? path)        ; -> <bool>
  (not (and-let* ((i (string-index path #\:))
                  ((not (string-index path #\/ 0 i)))))))

(define sync-times-file-name ".sync-times")
(define sync-ignore-file-name ".sync-ignore")

(define (saved-file-times-file local-path)
  (string-append local-path "/" sync-times-file-name))

(define (ignored-filename-globs local-path)
  (or (false-if-exception
       (call-with-input-file
           (string-append local-path "/" sync-ignore-file-name)
         read))
      '()))

(define (saved-file-times local-path)
  (or (false-if-exception
       (call-with-input-file
           (saved-file-times-file local-path)
         read))
      '()))

(define (write-saved-file-times times local-path)
  (call-with-output-file
      (saved-file-times-file local-path)
    (cut write times <>)))

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

(define (nth x) (lambda a (list-ref a x)))

(define (local-file-list local-path)
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
  (map (lambda (fn)
         (cons (string-trim-both
                (string-drop fn (string-length local-path))
                file-name-separator?)
               (local-mtime fn)))
       (local-file-list local-path)))

(define (file-times path)
  ((if (path-local? path)
       local-file-times
       rclone-file-times)
   path))

(define unix-time-comparison-thresh 2)

(define (unix-time~=? a b)
  (<= (abs (- a b)) unix-time-comparison-thresh))

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

(define* (all-files a-tab b-tab #:optional (ignore-glob-list '()))
  (filter (lambda (fn)
            (not (any (cute glob-match <> (basename fn))
                      ignore-glob-list)))
          (lset-difference string=?
                           (lset-union string=?
                                       (map car a-tab)
                                       (map car b-tab))
                           (list sync-times-file-name
                                 sync-ignore-file-name))))

(define (rclone-copy-file rclone-path new-local-path)
  (system* "rclone" "copyto" rclone-path new-local-path))

(define copy rclone-copy-file)
(define do-sync copy)

(define (sync src dest)
  (format #t "sync ~s -> ~s~%" src dest)
  (do-sync src dest))

(define (replace-in-string s l b)
  (regexp-substitute/global #f
                            (string-append "\\$" (string l))
                            s
                            'pre b 'post))

(define (diff a-orig b-orig)
  (format #t "diff ~s <-> ~s " a-orig b-orig)
  (let ((a (tmpnam))
        (b (tmpnam))
        (o (tmpnam))
        (merge-cmd (or (getenv "SYNCDIR_MERGE")
                       (string-append
                        (or (getenv "EDITOR")
                            (getenv "VISUAL")
                            "vi")
                        " $A $B $O"))))
    (copy a-orig a)
    (copy b-orig b)
    (let* ((real-merge-cmd
            (replace-in-string
             (replace-in-string
              (replace-in-string
               merge-cmd
               #\A a)
              #\B b)
             #\O o))
           (st
            (and (and=> (status:exit-val
                         (system real-merge-cmd))
                        zero?)
                 (access? o R_OK))))
      (delete-file a)
      (delete-file b)
      (if st
          (begin
            (display "done")
            (newline)
            o)
          (begin
            (false-if-exception
             (delete-file o))
            (display "cancelled")
            (newline)
            #f)))))

;;; replace with hash-tables
(define tab-ref assoc-ref)

(define (place-file-path place file)
  (string-append place "/" file))

;;; direction -> sync and record time
;;; else-> copy both to /tmp and run diff
;;; diff -> save this one to both places
;;; else-> skip
(define (process-file saved-tab a-tab b-tab a-place b-place fname)
  (let ((s (tab-ref saved-tab fname))
        (a (tab-ref a-tab fname))
        (b (tab-ref b-tab fname))
        (af (place-file-path a-place fname))
        (bf (place-file-path b-place fname)))
    (case (sync-direction s a b)
      ((a->b)
       (sync af bf)
       a)
      ((b->a)
       (sync bf af)
       b)
      ((both)
       (let ((out-fname (diff af bf)))
         (if out-fname
             (begin
               (copy out-fname af)
               (copy out-fname bf)
               (let ((mtime (stat:mtime (stat out-fname))))
                 (delete-file out-fname)
                 mtime))
             s)))
      (else s))))


(define (string-starts-with? s ss)
  (and (>= (string-length s) (string-length ss))
       (zero? (string-contains s ss 0 (string-length ss)))))

(define (relpath path)
  (let ((cwd (getcwd)))
    (if (string-starts-with? path cwd)
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

(define (main argv)
  (if (= (length argv) 3)
      (let-values (((local remote) (partition path-local? (cdr argv))))
        (if (or (null? local) (null? remote))
            (format #t "~a: error: there must be one local and one remote path~%"
                    (car argv))
            (let* ((local-path (relpath (realpath (car local))))
                   (a local-path)
                   (b (car remote))
                   (saved-tab (saved-file-times local-path))
                   (ignore-globs (ignored-filename-globs local-path))
                   (at (file-times a))
                   (bt (file-times b))
                   (new-saved-tab
                    (let loop ((new-tab '())
                               (files (all-files at bt ignore-globs)))
                      (if (null? files) new-tab
                          (let* ((f (car files))
                                 (new-times (process-file
                                             saved-tab at bt a b f)))
                            (loop (acons f new-times new-tab)
                                  (cdr files)))))))
              (write-saved-file-times new-saved-tab local-path))))
      (format #t "usage: ~a path-a path-b~%" (car argv))))
