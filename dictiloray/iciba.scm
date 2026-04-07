;;; Fetch suggestions from iCiba mobile API (HTTPS).

(define-module (dictiloray iciba)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (iciba-fetch-suggest))

(define %user-agent "dictiloray/0.2 (Guile CLI dictionary)")

(define (%wait-status->exit-code st)
  "close-pipe 返回 Linux wait 状态；不用 (ice-9 posix) 以便 Guix 等环境可编译。"
  (if (zero? st) 0 (ash (logand st #xff00) -8)))

(define (iciba-fetch-via-curl uri)
  "HTTPS when Guile lacks GnuTLS: delegate TLS to curl."
  (let ((port (open-pipe* OPEN_READ "curl" "-sS" "-f" "-L"
                            "-A" %user-agent uri)))
    (let ((body (get-string-all port)))
      (let ((st (close-pipe port)))
        (unless (zero? (%wait-status->exit-code st))
          (error "iciba: curl 失败（请确认已安装 curl 且可访问网络）" st))
        body))))

(define (iciba-fetch-suggest word)
  (let* ((enc (uri-encode word))
         (uri (string-append
                "https://dict-mobile.iciba.com/interface/index.php?"
                "c=word&m=getsuggest&is_need_mean=1&nums=8&word="
                enc)))
    (catch
      'gnutls-not-available
      (lambda ()
        (call-with-values
          (lambda ()
            (http-get uri
                      #:headers `((user-agent . ,%user-agent))))
          (lambda (response body)
            (unless (= (response-code response) 200)
              (error "iciba: HTTP status" (response-code response)))
            (unless (string? body)
              (error "iciba: expected string body"))
            body)))
      (lambda (key . _)
        (catch #t
          (lambda () (iciba-fetch-via-curl uri))
          (lambda (k . a)
            (error
             (string-append
              "HTTPS 不可用：Guile 未加载 (gnutls)，且 curl 回退失败。"
              " 请安装 guile-gnutls，或安装 curl 并保证其在 PATH 中。")
             k a)))))))
