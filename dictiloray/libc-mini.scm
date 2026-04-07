;;; Tiny libc wrappers for Guile builds without (ice-9 posix).

(define-module (dictiloray libc-mini)
  #:use-module (system foreign)
  #:export (libc-getenv libc-time-seconds libc-file-exists? libc-mkdir
            libc-isatty))

(define %libc #f)

(define (ensure-libc!)
  (unless %libc
    (set! %libc (catch #t (lambda () (dynamic-link)) (lambda _ #f)))
    (unless %libc
      (error "dictiloray: dynamic-link() failed; cannot use libc-mini"))))

(define (libc-getenv name)
  (ensure-libc!)
  (let ((f (pointer->procedure '* (dynamic-func "getenv" %libc) (list '*))))
    (let ((p (f (string->pointer name))))
      (if (null-pointer? p) #f (pointer->string p)))))

(define (libc-time-seconds)
  "Return Unix epoch seconds (like time(NULL))."
  (ensure-libc!)
  (let ((f (pointer->procedure int (dynamic-func "time" %libc) (list '*))))
    (f %null-pointer)))

(define F-OK 0)

(define (libc-file-exists? path)
  (ensure-libc!)
  (let ((access (pointer->procedure int (dynamic-func "access" %libc) (list '* int))))
    (zero? (access (string->pointer path) F-OK))))

(define (libc-mkdir path mode)
  (ensure-libc!)
  (let ((mk (pointer->procedure int (dynamic-func "mkdir" %libc) (list '* int))))
    (zero? (mk (string->pointer path) mode))))

(define (libc-isatty fd)
  "非零若 fd 为交互式终端（用于决定是否启用 ANSI 颜色）。"
  (ensure-libc!)
  (let ((f (pointer->procedure int (dynamic-func "isatty" %libc) (list int))))
    (not (zero? (f fd)))))
