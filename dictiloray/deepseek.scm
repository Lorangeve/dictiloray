;;; DeepSeek Chat API: sentence translation (JSON mode).

(define-module (dictiloray deepseek)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (dictiloray json-minimal)
  #:use-module (dictiloray libc-mini)
  #:export (deepseek-api-key-from-env
            deepseek-base-url
            deepseek-translate+rare
            deepseek-translate+rare/values))

(define %user-agent "dictiloray/0.2 (Guile CLI dictionary)")

(define (deepseek-api-key-from-env)
  (libc-getenv "DEEPSEEK_API_KEY"))

(define (deepseek-base-url)
  (let ((b (libc-getenv "DEEPSEEK_API_BASE")))
    (if (and b (not (string-null? (string-trim-both b))))
        (string-trim-right (string-trim-both b) #\/)
        "https://api.deepseek.com")))

(define (%wait-status->exit-code st)
  (if (zero? st) 0 (ash (logand st #xff00) -8)))

(define (ds-split-body+http-code all)
  (let ((nl (string-rindex all #\newline)))
    (if (not nl)
        (values all 0)
        (let* ((tail (string-trim-both (substring all (+ nl 1))))
               (code (string->number tail)))
          (if code
              (values (substring all 0 nl) code)
              (values all 0))))))

(define (ds-temp-json-path)
  (string-append (or (libc-getenv "TMPDIR")
                     (libc-getenv "TEMP")
                     "/tmp")
                 "/dictiloray-ds-"
                 (number->string (libc-time-seconds))
                 "-"
                 (number->string (random 100000))
                 ".json"))

(define (ds-post-via-curl uri json-body bearer)
  (let* ((path (ds-temp-json-path))
         (_ (call-with-output-file path (lambda (p) (display json-body p))))
         (port (open-pipe* OPEN_READ "curl" "-sS" "-X" "POST" uri
                            "-H" "Content-Type: application/json"
                            "-H" (string-append "Authorization: Bearer " bearer)
                            "-A" %user-agent
                            "-w" "\n%{http_code}"
                            "-d" (string-append "@" path)))
         (all (get-string-all port))
         (st (close-pipe port)))
    (catch #t (lambda () (delete-file path)) (lambda _ #f))
    (unless (zero? (%wait-status->exit-code st))
      (error "deepseek: curl 失败" st))
    (receive (body code)
        (ds-split-body+http-code all)
      (values (or code 0) body))))

(define (ds-http-post uri json-body bearer)
  (catch
    'gnutls-not-available
    (lambda ()
      (call-with-values
        (lambda ()
          (http-post uri
                     #:body json-body
                     #:headers `((Content-Type . "application/json")
                                 (Authorization . ,(string-append "Bearer " bearer))
                                 (User-Agent . ,%user-agent))))
        (lambda (response body)
          (values (response-code response)
                  (if (string? body) body "")))))
    (lambda (key . _)
      (catch #t
        (lambda () (ds-post-via-curl uri json-body bearer))
        (lambda (k . a)
          (error "deepseek: HTTPS 不可用（需 guile-gnutls 或可用 curl）" k a))))))

(define (alist-ref al key)
  (cond ((assoc key al) => cdr) (else #f)))

(define %system-prompt-translation-only
  (string-append
   "You must output a single JSON object only, no markdown fences."
   " Single key: \"translation\" — natural Chinese rendering of the user's text;"
   " if the text is already Chinese, give a concise paraphrase or gloss in Chinese."
   " No other keys."))

(define %system-prompt-with-rare
  (string-append
   "You must output a single JSON object only, no markdown fences."
   " Keys: \"translation\" — natural Chinese rendering of the user's text;"
   " if the text is already Chinese, give a concise paraphrase or gloss in Chinese."
   " \"rare_words\" — JSON array of at most 8 strings: relatively uncommon or"
   " advanced lemmas from the user's text (English: exclude common function words;"
   " prefer words a learner would look up; Chinese: multi-character terms ok)."
   " Only items that clearly appear in or correspond to the text. Use [] if none."))

(define (deepseek-request-json sentence with-rare?)
  (scm->json-string
   `(("model" . "deepseek-chat")
     ("messages" .
       #(,(list (cons "role" "system")
                (cons "content"
                      (if with-rare?
                          %system-prompt-with-rare
                          %system-prompt-translation-only)))
         ,(list (cons "role" "user") (cons "content" sentence))))
     ("response_format" . (("type" . "json_object"))))))

(define (string-head s n)
  (let ((len (string-length s)))
    (if (<= len n) s (substring s 0 n))))

(define (extract-json-object s)
  (let ((i (string-index s #\{))
        (j (string-rindex s #\})))
    (and i j (>= j i) (substring s i (+ j 1)))))

(define (strip-markdown-json-fence s)
  (let ((t (string-trim-both s)))
    (if (not (string-prefix? "```" t))
        t
        (let* ((p (string-index t #\newline))
               (after (if p (string-trim-both (substring t (+ p 1))) ""))
               (q (string-contains after "```")))
          (if q
              (string-trim-both (substring after 0 q))
              (string-trim-both after))))))

(define (parse-model-json content)
  (let* ((t (strip-markdown-json-fence content))
         (slice (or (extract-json-object t) (string-trim-both t))))
    (catch #t
      (lambda () (json-string->scm slice))
      (lambda _ #f))))

(define (vector->string-list v)
  (if (not (vector? v))
      '()
      (filter-map
       (lambda (x)
         (and (string? x)
              (let ((t (string-trim-both x)))
                (and (not (string-null? t)) t))))
       (vector->list v))))

(define (rare-words-normalize rw)
  (cond ((vector? rw) (vector->string-list rw))
        ((list? rw)
         (filter-map
          (lambda (x)
            (and (string? x)
                 (let ((t (string-trim-both x)))
                   (and (not (string-null? t)) t))))
          rw))
        (else '())))

(define (format-api-err e)
  (cond ((and (pair? e) (string? (alist-ref e "message")))
         (alist-ref e "message"))
        ((string? e) e)
        (else
         (catch #t
           (lambda () (string-head (scm->json-string e) 200))
           (lambda _ "（无法序列化 error 对象）")))))

(define (deepseek-http-reason code body)
  (let ((api-msg (and (string? body)
                      (> (string-length body) 0)
                      (catch #t
                        (lambda ()
                          (let ((j (json-string->scm body)))
                            (and (pair? j)
                                 (let ((e (alist-ref j "error")))
                                   (and (pair? e) (alist-ref e "message"))))))
                        (lambda _ #f)))))
    (string-append "HTTP " (number->string code)
                   (cond ((and api-msg (string? api-msg))
                          (string-append ": " api-msg))
                         ((and (string? body) (> (string-length body) 0))
                          (string-append " — " (string-head body 160)))
                         (else "")))))

(define (deepseek-inner-to-values inner with-rare?)
  (let ((tr (alist-ref inner "translation")))
    (if (not (string? tr))
        (values #f #f "模型 JSON 缺少有效的字符串字段 translation")
        (if with-rare?
            (let ((rw (alist-ref inner "rare_words")))
              (values #t
                      (list (cons "translation" (string-trim-both tr))
                            (cons "rare_words" (rare-words-normalize rw)))
                      ""))
            (values #t
                    (list (cons "translation" (string-trim-both tr)))
                    "")))))

(define* (deepseek-translate+rare/values sentence
                                          #:key (api-key #f) (base-url #f) (with-rare? #f))
  "成功 (values #t alist \"\")；失败 (values #f #f 原因说明)。
with-rare? 为 #f 时仅 translation；为 #t 时另含 rare_words（字符串列表）。"
  (let ((key (or api-key (deepseek-api-key-from-env)))
        (base (or base-url (deepseek-base-url))))
    (cond
      ((or (not key) (string-null? (string-trim-both key)))
       (values #f #f "未设置 DEEPSEEK_API_KEY 或密钥为空"))
      (else
       (let* ((uri (string-append base "/chat/completions"))
              (payload (deepseek-request-json sentence with-rare?))
              (key-t (string-trim-both key)))
         (receive (code body)
             (ds-http-post uri payload key-t)
           (cond
             ((not (= code 200))
              (values #f #f (deepseek-http-reason code body)))
             ((or (not (string? body)) (string-null? body))
              (values #f #f "服务器返回空正文"))
             (else
              (let ((top (catch #t (lambda () (json-string->scm body)) (lambda _ #f))))
                (cond
                  ((not (pair? top))
                   (values #f #f
                           (string-append "无法解析 DeepSeek 顶层 JSON: "
                                          (string-head body 120))))
                  ((alist-ref top "error")
                   => (lambda (e)
                        (values #f #f
                                (string-append "API 错误: " (format-api-err e)))))
                  (else
                   (let ((choices (alist-ref top "choices")))
                     (cond
                       ((not (and (vector? choices) (> (vector-length choices) 0)))
                        (values #f #f "响应中无 choices 或数组为空"))
                       (else
                        (let* ((c0 (vector-ref choices 0))
                               (msg (and (pair? c0) (alist-ref c0 "message")))
                               (content (and (pair? msg) (alist-ref msg "content"))))
                          (cond
                            ((or (not content) (json-null? content))
                             (values #f #f "模型未返回正文（content 为空或 null）"))
                            ((not (string? content))
                             (values #f #f "模型返回的 content 不是字符串"))
                            (else
                             (let ((inner (parse-model-json content)))
                               (cond
                                 ((not (pair? inner))
                                  (values #f #f
                                          (string-append "无法解析模型 JSON: "
                                                         (string-head (string-trim-both content)
                                                                      100))))
                                 (else
                                  (deepseek-inner-to-values inner with-rare?)))))))))))))))))))))

(define* (deepseek-translate+rare sentence #:key (api-key #f) (base-url #f) (with-rare? #f))
  "成功返回 alist（translation，可选 rare_words）；失败返回 #f。"
  (receive (ok data _) (deepseek-translate+rare/values sentence
                                                       #:api-key api-key
                                                       #:base-url base-url
                                                       #:with-rare? with-rare?)
    (and ok data)))
