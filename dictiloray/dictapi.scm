;;; Free Dictionary API (https://dictionaryapi.dev/) — IPA + example sentences.

(define-module (dictiloray dictapi)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (enrich-eligible-key?
            dictapi-fetch-en
            dictapi-entries-vector?
            dictapi-collect-phonetics
            dictapi-collect-examples))

(define %user-agent "dictiloray/0.2 (Guile CLI dictionary)")

(define (%wait-status->exit-code st)
  (if (zero? st) 0 (ash (logand st #xff00) -8)))

(define (dictapi-split-body+http-code all)
  "curl -w 在末尾追加换行与状态码；正文可能含换行，故从末尾取纯数字行。"
  (let ((nl (string-rindex all #\newline)))
    (if (not nl)
        (values all 0)
        (let* ((tail (string-trim-both (substring all (+ nl 1))))
               (code (string->number tail)))
          (if code
              (values (substring all 0 nl) code)
              (values all 0))))))

(define (dictapi-fetch-via-curl uri)
  (let ((port (open-pipe* OPEN_READ "curl" "-sS" "-w" "\n%{http_code}"
                            "-A" %user-agent uri)))
    (let* ((all (get-string-all port))
           (st (close-pipe port)))
      (unless (zero? (%wait-status->exit-code st))
        (error "dictapi: curl 失败" st))
      (receive (body code)
          (dictapi-split-body+http-code all)
        (values (or code 0) body)))))

(define (http-get-status+body uri)
  (catch
    'gnutls-not-available
    (lambda ()
      (call-with-values
        (lambda ()
          (http-get uri #:headers `((user-agent . ,%user-agent))))
        (lambda (response body)
          (values (response-code response)
                  (if (string? body) body "")))))
    (lambda (key . _)
      (catch #t
        (lambda () (dictapi-fetch-via-curl uri))
        (lambda (k . a)
          (error
           "dictapi: HTTPS 不可用（需 guile-gnutls 或可用 curl）"
           k a))))))

(define (contains-cjk? s)
  (string-any
   (lambda (c)
     (let ((i (char->integer c)))
       (or (and (>= i #x4E00) (<= i #x9FFF))
           (and (>= i #x3040) (<= i #x30FF))
           (and (>= i #xAC00) (<= i #xD7AF)))))
   s))

(define (enrich-eligible-key? s)
  (and (string? s)
       (let ((t (string-trim-both s)))
         (and (not (string-null? t))
              (not (contains-cjk? t))))))

(define (dictapi-fetch-en word)
  (let* ((enc (uri-encode (string-trim-both word)))
         (uri (string-append
                "https://api.dictionaryapi.dev/api/v2/entries/en/" enc)))
    (receive (code body)
        (http-get-status+body uri)
      (cond ((= code 404) #f)
            ((not (= code 200)) #f)
            (else body)))))

(define (dictapi-entries-vector? data)
  (and (vector? data) (positive? (vector-length data))))

(define (alist-ref al key)
  (cond ((assoc key al) => cdr) (else #f)))

(define (phonetic-texts-from-entry entry)
  (if (not (pair? entry))
      '()
      (let ((ph (alist-ref entry "phonetics")))
        (if (not (vector? ph))
            '()
            (filter-map
             (lambda (p)
               (and (pair? p)
                    (let ((t (alist-ref p "text")))
                      (and (string? t)
                           (let ((u (string-trim-both t)))
                             (and (not (string-null? u)) u))))))
             (vector->list ph))))))

(define (dictapi-collect-phonetics entries)
  (if (not (dictapi-entries-vector? entries))
      '()
      (let ((first (vector-ref entries 0)))
        (let lp ((xs (phonetic-texts-from-entry first)) (seen '()) (out '()))
          (cond ((null? xs) (reverse out))
                ((member (car xs) seen) (lp (cdr xs) seen out))
                (else (lp (cdr xs) (cons (car xs) seen) (cons (car xs) out))))))))

(define (definition-examples-from-meaning m)
  (if (not (pair? m))
      '()
      (let ((defs (alist-ref m "definitions")))
        (if (not (vector? defs))
            '()
            (filter-map
             (lambda (d)
               (and (pair? d)
                    (let ((ex (alist-ref d "example")))
                      (and (string? ex)
                           (let ((t (string-trim-both ex)))
                             (and (not (string-null? t)) t))))))
             (vector->list defs))))))

(define (dictapi--entry-example-strings entry)
  (if (not (pair? entry))
      '()
      (let ((means (alist-ref entry "meanings")))
        (if (not (vector? means))
            '()
            (append-map definition-examples-from-meaning (vector->list means))))))

(define* (dictapi-collect-examples entries #:optional (max-n 8))
  (if (not (dictapi-entries-vector? entries))
      '()
      (let ((flat (append-map dictapi--entry-example-strings (vector->list entries))))
        (let lp ((xs flat) (acc '()) (seen '()))
          (cond ((or (null? xs) (>= (length acc) max-n)) (reverse acc))
                ((member (car xs) seen) (lp (cdr xs) acc seen))
                (else (lp (cdr xs) (cons (car xs) acc) (cons (car xs) seen))))))))
