#!/usr/bin/env -S guile --no-auto-compile -s
!#

;;; CLI: 金山词霸移动接口 + SQLite 缓存（纯 Guile）。
;;; 英文词条（词条 key 无中日韩）额外拉取 dictionaryapi.dev 的 IPA 音标与例句。
;;; 依赖：libsqlite3；HTTPS 优先 Guile+(gnutls)，否则自动用 curl。

(eval-when (compile load eval)
  (define (%script-dir)
    (let ((f (current-filename)))
      (if (not f)
          "."
          (let ((i (string-rindex f #\/)))
            (if i (substring f 0 i) ".")))))
  (set! %load-path (cons (%script-dir) %load-path)))

(use-modules (ice-9 format)
             (ice-9 receive)
             (srfi srfi-13)
             (dictiloray cache)
             (dictiloray dictapi)
             (dictiloray format)
             (dictiloray iciba)
             (dictiloray json-minimal)
             (dictiloray libc-mini))

(define (default-db-path)
  (let ((base (or (libc-getenv "XDG_CACHE_HOME")
                  (if (libc-getenv "HOME")
                      (string-append (libc-getenv "HOME") "/.cache")
                      (error "dict: 请设置 HOME 或 XDG_CACHE_HOME")))))
    (string-append base "/dictiloray/cache.sqlite3")))

(define (normalize-key word)
  (string-downcase (string-trim-both word)))

(define (en-dictapi-cache-key entry-key)
  (string-append (normalize-key entry-key) "\t" "en.dictapi"))

(define (json-with-en data phonetics examples)
  (if (and (null? phonetics) (null? examples))
      data
      (append data
              (list
               (cons "dictiloray_en"
                     (list (cons "phonetics" (list->vector phonetics))
                           (cons "examples" (list->vector examples))))))))

(define (fetch-enrichment db lk no-cache? refresh? now)
  (and (enrich-eligible-key? lk)
       (let* ((ck (en-dictapi-cache-key lk))
              (cached (and (not refresh?) (not no-cache?) (cache-get db ck)))
              (raw (or cached
                       (catch #t
                         (lambda () (dictapi-fetch-en lk))
                         (lambda _ #f)))))
         (and raw
              (let ((parsed (catch #t
                              (lambda () (json-string->scm raw))
                              (lambda _ #f))))
                (and parsed
                     (begin
                       (unless cached
                         (cache-set! db ck raw now))
                       parsed)))))))

(define (usage)
  (display "用法: dict [选项] <词>\n" (current-error-port))
  (display "  --json          输出 JSON（单行 + 换行，可管道到 jq）\n" (current-error-port))
  (display "  --color         强制 ANSI 颜色（即使 stdout 非终端）\n" (current-error-port))
  (display "  --no-color      禁用颜色（亦遵守环境变量 NO_COLOR）\n" (current-error-port))
  (display "  --no-cache      不读缓存（仍会写入）\n" (current-error-port))
  (display "  --refresh       强制联网并覆盖缓存\n" (current-error-port))
  (display "  --cache-db 路径 SQLite 数据库路径\n" (current-error-port))
  (display "  --clear-cache   清空缓存\n" (current-error-port))
  (display "  -h, --help      帮助\n" (current-error-port))
  (display "\n英文词条另显示音标/例句（dictionaryapi.dev）；--json 中见 dictiloray_en。\n"
           (current-error-port)))

(define (now-epoch)
  (libc-time-seconds))

(define (parse-args args)
  (let lp ((rest args)
           (json? #f)
           (no-cache? #f)
           (refresh? #f)
           (clear? #f)
           (db #f)
           (help? #f)
           (color-pref 'auto)
           (pos '()))
    (cond
      ((null? rest)
       (values json? no-cache? refresh? clear? db help? color-pref (reverse pos)))
      ((or (string=? (car rest) "-h") (string=? (car rest) "--help"))
       (lp (cdr rest) json? no-cache? refresh? clear? db #t color-pref pos))
      ((string=? (car rest) "--color")
       (lp (cdr rest) json? no-cache? refresh? clear? db help? 'always pos))
      ((string=? (car rest) "--no-color")
       (lp (cdr rest) json? no-cache? refresh? clear? db help? 'never pos))
      ((string=? (car rest) "--json")
       (lp (cdr rest) #t no-cache? refresh? clear? db help? color-pref pos))
      ((string=? (car rest) "--no-cache")
       (lp (cdr rest) json? #t refresh? clear? db help? color-pref pos))
      ((string=? (car rest) "--refresh")
       (lp (cdr rest) json? no-cache? #t clear? db help? color-pref pos))
      ((string=? (car rest) "--clear-cache")
       (lp (cdr rest) json? no-cache? refresh? #t db help? color-pref pos))
      ((string=? (car rest) "--cache-db")
       (if (null? (cdr rest))
           (error "dict: --cache-db 需要路径参数")
           (lp (cddr rest) json? no-cache? refresh? clear? (cadr rest) help? color-pref pos)))
      ((string-prefix? "--cache-db=" (car rest))
       (let ((p (substring (car rest) (string-length "--cache-db="))))
         (when (string-null? p)
           (error "dict: --cache-db= 需要路径"))
         (lp (cdr rest) json? no-cache? refresh? clear? p help? color-pref pos)))
      ((string-prefix? "--" (car rest))
       (error "dict: 未知选项" (car rest)))
      (else (lp (cdr rest) json? no-cache? refresh? clear? db help? color-pref (cons (car rest) pos))))))

(define (no-color-env?)
  (let ((v (libc-getenv "NO_COLOR")))
    (and v (not (string-null? v)))))

(define (use-terminal-color? color-pref)
  (cond ((eq? color-pref 'never) #f)
        ((eq? color-pref 'always) #t)
        (else (and (not (no-color-env?))
                   (libc-isatty (port->fdes (current-output-port)))))))

(define (main)
  (receive (json? no-cache? refresh? clear? db-path help? color-pref pos)
      (parse-args (cdr (program-arguments)))
    (when help?
      (usage)
      (primitive-exit 0))
    (let ((db (or db-path (default-db-path))))
      (when clear?
        (ensure-cache-parent-dir db)
        (let ((n (cache-clear! db)))
          (format (current-error-port) "已删除 ~a 条缓存。\n" n)
          (primitive-exit 0)))
      (when (null? pos)
        (usage)
        (primitive-exit 2))
      (when (not (null? (cdr pos)))
        (display "dict: 请只查询一个词（含空格请自行加引号）\n" (current-error-port))
        (primitive-exit 2))
      (let* ((word (car pos))
             (key (normalize-key word)))
        (ensure-cache-parent-dir db)
        (let* ((cached (and (not refresh?) (not no-cache?) (cache-get db key)))
               (raw
                (or cached
                    (catch #t
                      (lambda () (iciba-fetch-suggest word))
                      (lambda (k . a)
                        (format (current-error-port) "请求失败: ~a ~a\n" k a)
                        (primitive-exit 1))))))
          (unless cached
            (cache-set! db key raw (now-epoch)))
          (let ((data (catch #t
                        (lambda () (json-string->scm raw))
                        (lambda (k . a)
                          (format (current-error-port) "JSON 解析失败: ~a ~a\n" k a)
                          (primitive-exit 1)))))
            (let* ((entry (pick-best-entry word data))
                   (lk (and entry (cond ((assoc "key" entry) => cdr) (else #f))))
                   (en (and lk (fetch-enrichment db lk no-cache? refresh? (now-epoch))))
                   (phonetics (if (dictapi-entries-vector? en)
                                  (dictapi-collect-phonetics en)
                                  '()))
                   (examples (if (dictapi-entries-vector? en)
                                 (dictapi-collect-examples en)
                                 '())))
              (cond
                (json?
                 (display (scm->json-pretty-string
                           (json-with-en data phonetics examples))))
                (else
                 (unless entry
                   (format (current-error-port) "未解析到释义: ~a\n" data)
                   (primitive-exit 1))
                 (display (format-entry entry
                                        #:color? (use-terminal-color? color-pref)
                                        #:phonetics phonetics
                                        #:examples examples))
                 (newline))))))))))

(main)
