;;; Pick best suggestion row and format for terminal.

(define-module (dictiloray format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (pick-best-entry format-entry format-section-banner format-lookup-result))

(define %esc (string #\x1b))

(define %rst (string-append %esc "[0m"))
(define %dim (string-append %esc "[2m"))
(define %key (string-append %esc "[1;96m"))   ; bold bright cyan
(define %par (string-append %esc "[2;39m"))   ; dim default
(define %pos (string-append %esc "[1;33m"))   ; bold yellow 词性
(define %empty (string-append %esc "[2;31m")) ; dim red (无释义)
(define %ipa (string-append %esc "[1;35m"))   ; bold magenta 音标
(define %lbl (string-append %esc "[1;97m"))   ; bold bright white 字段标签（深色背景下可读）

(define (cwrap seq s)
  (string-append seq s %rst))

(define (format-lookup-count-suffix n color?)
  (let ((s (format #f " ×~a" n)))
    (if color? (cwrap %dim s) s)))

(define* (format-section-banner title #:key (color? #f))
  (let* ((n (string-length title))
         (u-len (min (max n 2) 56)))
    (if color?
        (string-append (cwrap %key title) "\n" (cwrap %dim (make-string u-len #\─)))
        (string-append title "\n" (make-string u-len #\-)))))

(define (alist-ref al key)
  (cond ((assoc key al) => cdr) (else #f)))

(define (string-down s)
  (string-downcase s))

(define (pick-best-entry word data)
  (and (pair? data)
       (let ((s (alist-ref data "status")))
         (and (number? s) (= s 1)))
       (let ((msg (alist-ref data "message")))
         (and (vector? msg)
              (positive? (vector-length msg))
              (let ((w (string-down (string-trim-both word))))
                (or
                  (vector-find msg
                               (lambda (item)
                                 (and (pair? item)
                                      (let ((k (alist-ref item "key")))
                                        (and (string? k)
                                             (string=? (string-down (string-trim-both k))
                                                       w))))))
                  (vector-ref msg 0)))))))

(define (vector-find vec pred)
  (let ((n (vector-length vec)))
    (let lp ((i 0))
      (cond ((>= i n) #f)
            ((pred (vector-ref vec i)) (vector-ref vec i))
            (else (lp (1+ i)))))))

(define (entry-key-normalized item)
  (and (pair? item)
       (let ((k (alist-ref item "key")))
         (and (string? k) (string-downcase (string-trim-both k))))))

(define (block-glosses block)
  "从一条 means 块取出非空义项字符串列表。"
  (if (not (pair? block))
      '()
      (let ((ml (alist-ref block "means")))
        (if (not (vector? ml))
            '()
            (filter (lambda (x)
                      (and (string? x)
                           (not (string-null? (string-trim-both x)))))
                    (vector->list ml))))))

(define (entry-has-structured-means? item)
  "是否有按词性拆开的义项；有则不必再显示 paraphrase（避免与 API 摘要行重复）。"
  (let ((means (alist-ref item "means")))
    (and (vector? means)
         (any (lambda (b) (not (null? (block-glosses b))))
              (vector->list means)))))

(define* (format-entry item #:key (color? #f) (phonetics '()) (examples '())
                        (lookup-count-beside? #f) (lookup-count #f))
  (unless (pair? item) (error "format-entry: bad row"))
  (let* ((rows '())
         (emit (lambda (s) (set! rows (append rows (list s)))))
         (fmt-key (if color? (lambda (s) (cwrap %key s)) identity))
         (fmt-par (if color? (lambda (s) (cwrap %par s)) identity))
         (fmt-pos (if color? (lambda (s) (cwrap %pos s)) identity))
         (fmt-ipa (if color? (lambda (s) (cwrap %ipa s)) identity))
         (fmt-lbl (if color? (lambda (s) (cwrap %lbl s)) identity))
         (indent "    ")
         (sep "\n\n"))
    (cond ((alist-ref item "key")
           => (lambda (k)
                (when (and (string? k) (not (string-null? k)))
                  (emit
                   (let ((head (fmt-key k)))
                     (if (and lookup-count-beside?
                              (number? lookup-count)
                              (> lookup-count 0))
                         (string-append head (format-lookup-count-suffix lookup-count color?))
                         head)))))))
    (when (pair? phonetics)
      (emit (string-append (fmt-lbl "音标") "  "
                           (fmt-ipa (string-join phonetics " · ")))))
    (cond ((alist-ref item "paraphrase")
           => (lambda (p)
                (when (and (not (entry-has-structured-means? item))
                           (string? p)
                           (not (string-null? (string-trim-both p))))
                  (emit (fmt-par (string-trim-both p)))))))
    (let ((means (alist-ref item "means")))
      (when (vector? means)
        (let ((n (vector-length means)))
          (let lp ((i 0))
            (when (< i n)
              (let ((block (vector-ref means i)))
                (when (pair? block)
                  (let* ((part (or (alist-ref block "part") ""))
                         (glosses (block-glosses block)))
                    (unless (null? glosses)
                      (let ((joined (string-join glosses "；")))
                        (emit (string-append indent
                                (if (string-null? part)
                                    joined
                                    (string-append (fmt-pos part) " " joined))))))))
              (lp (1+ i)))))))
    (when (pair? examples)
      (emit (fmt-lbl "例句"))
      (for-each (lambda (ex) (emit (string-append indent ex))) examples))
    (cond
      ((null? rows)
       (if color? (cwrap %empty "(无释义)") "(无释义)"))
      ((null? (cdr rows))
       (car rows))
      (else
       (string-join rows sep))))))

(define* (format-lookup-result word data #:key (color? #f) (phonetics '()) (examples '()) (verbose? #f)
          (lookup-count-beside? #f) (lookup-count #f) (lookup-count-get #f))
  (let ((best (pick-best-entry word data))
        (msg (and (pair? data) (alist-ref data "message"))))
    (cond
      ((not (vector? msg)) "(无释义)")
      ((zero? (vector-length msg)) "(无释义)")
      (else
       (let ((best-key (and best (entry-key-normalized best)))
             (between "\n\n\n"))
         (if verbose?
             (string-join
              (let lp ((i 0) (acc '()))
                (if (>= i (vector-length msg))
                    (reverse acc)
                    (let ((item (vector-ref msg i)))
                      (lp (1+ i)
                          (if (pair? item)
                              (cons
                               (format-entry item
                                             #:color? color?
                                             #:phonetics
                                             (if (and best-key
                                                      (equal? (entry-key-normalized item) best-key))
                                                 phonetics
                                                 '())
                                             #:examples
                                             (if (and best-key
                                                      (equal? (entry-key-normalized item) best-key))
                                                 examples
                                                 '())
                                             #:lookup-count-beside? lookup-count-beside?
                                             #:lookup-count
                                             (if (and lookup-count-beside? lookup-count-get)
                                                 (let ((k (entry-key-normalized item)))
                                                   (if (string? k) (lookup-count-get k) 0))
                                                 0))
                               acc)
                              acc)))))
              between)
             (if best
                 (format-entry best
                               #:color? color?
                               #:phonetics phonetics
                               #:examples examples
                               #:lookup-count-beside? lookup-count-beside?
                               #:lookup-count (if lookup-count-beside? lookup-count #f))
                 "(无释义)")))))))
