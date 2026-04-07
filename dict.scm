#!/usr/bin/env -S guile --no-auto-compile -s
!#

;;; CLI: 金山词霸移动接口 + SQLite 缓存（纯 Guile）。
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

(define (usage)
  (display "用法: dict [选项] <词>\n" (current-error-port))
  (display "  --json          输出 JSON（单行 + 换行，可管道到 jq）\n" (current-error-port))
  (display "  --no-cache      不读缓存（仍会写入）\n" (current-error-port))
  (display "  --refresh       强制联网并覆盖缓存\n" (current-error-port))
  (display "  --cache-db 路径 SQLite 数据库路径\n" (current-error-port))
  (display "  --clear-cache   清空缓存\n" (current-error-port))
  (display "  -h, --help      帮助\n" (current-error-port)))

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
           (pos '()))
    (cond
      ((null? rest)
       (values json? no-cache? refresh? clear? db help? (reverse pos)))
      ((or (string=? (car rest) "-h") (string=? (car rest) "--help"))
       (lp (cdr rest) json? no-cache? refresh? clear? db #t pos))
      ((string=? (car rest) "--json")
       (lp (cdr rest) #t no-cache? refresh? clear? db help? pos))
      ((string=? (car rest) "--no-cache")
       (lp (cdr rest) json? #t refresh? clear? db help? pos))
      ((string=? (car rest) "--refresh")
       (lp (cdr rest) json? no-cache? #t clear? db help? pos))
      ((string=? (car rest) "--clear-cache")
       (lp (cdr rest) json? no-cache? refresh? #t db help? pos))
      ((string=? (car rest) "--cache-db")
       (if (null? (cdr rest))
           (error "dict: --cache-db 需要路径参数")
           (lp (cddr rest) json? no-cache? refresh? clear? (cadr rest) help? pos)))
      ((string-prefix? "--cache-db=" (car rest))
       (let ((p (substring (car rest) (string-length "--cache-db="))))
         (when (string-null? p)
           (error "dict: --cache-db= 需要路径"))
         (lp (cdr rest) json? no-cache? refresh? clear? p help? pos)))
      ((string-prefix? "--" (car rest))
       (error "dict: 未知选项" (car rest)))
      (else (lp (cdr rest) json? no-cache? refresh? clear? db help? (cons (car rest) pos))))))

(define (main)
  (receive (json? no-cache? refresh? clear? db-path help? pos)
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
            (cond
              (json?
               (display (scm->json-pretty-string data)))
              (else
               (let ((entry (pick-best-entry word data)))
                 (unless entry
                   (format (current-error-port) "未解析到释义: ~a\n" data)
                   (primitive-exit 1))
                 (display (format-entry entry))
                 (newline))))))))))

(main)
