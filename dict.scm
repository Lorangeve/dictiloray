#!/usr/bin/env -S guile --no-auto-compile -s
!#

;;; CLI: 金山词霸移动接口 + SQLite 缓存（纯 Guile）。
;;; 英文词条（词条 key 无中日韩）额外拉取 dictionaryapi.dev 的 IPA 音标与例句。
;;; 句子模式：DeepSeek 译文；可选 --rare 生僻词+查词（需 DEEPSEEK_API_KEY）。
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
             (ice-9 match)
             (ice-9 receive)
             (srfi srfi-1)
             (srfi srfi-13)
             (dictiloray cache)
             (dictiloray deepseek)
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

(define (deepseek-cache-key sentence rare?)
  (string-append "deepseek\t" (if rare? "rare\t" "") sentence))

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
  (display "用法: dict [选项] <词或句子>\n" (current-error-port))
  (display "  --sentence      强制按句子处理（否则自动识别：多空格词、标点等）\n"
           (current-error-port))
  (display "  --rare          句子模式：生僻词列表 + 金山释义（默认关闭，仅译文）\n"
           (current-error-port))
  (display "  --json          输出 JSON（单行 + 换行，可管道到 jq）\n" (current-error-port))
  (display "  --verbose, -v   更全：更多 iCiba 候选条数与英文例句；文本模式列出全部建议词条\n"
           (current-error-port))
  (display "  --color         强制 ANSI 颜色（即使 stdout 非终端）\n" (current-error-port))
  (display "  --no-color      禁用颜色（亦遵守环境变量 NO_COLOR）\n" (current-error-port))
  (display "  --no-cache      不读缓存（仍会写入）\n" (current-error-port))
  (display "  --refresh       强制联网并覆盖缓存\n" (current-error-port))
  (display "  --cache-db 路径 SQLite 数据库路径\n" (current-error-port))
  (display "  --clear-cache   清空缓存（不重置查词次数 lookup_stats）\n" (current-error-port))
  (display "  --count-beside  在词条标题旁显示 ×次数（不重复文末「查阅 n 次」）\n"
           (current-error-port))
  (display "  --top N         列出查词次数最高的 N 个词（仅统计，不可与查词同用）\n"
           (current-error-port))
  (display "  -h, --help      帮助\n" (current-error-port))
  (display "\n英文单词另显示音标/例句（dictionaryapi.dev）；--json 中见 dictiloray_en。\n"
           (current-error-port))
  (display "单词模式：每次成功拉取释义后累计查词次数（小写归一化），文末与 JSON 字段 lookup_count。\n"
           (current-error-port))
  (display "句子：DeepSeek 默认只译；加 --rare 则要生僻词并查金山（需 DEEPSEEK_API_KEY；\n"
           (current-error-port))
  (display "可选 DEEPSEEK_API_BASE，默认 https://api.deepseek.com）。\n"
           (current-error-port)))

(define (now-epoch)
  (libc-time-seconds))

(define (parse-positive-int s label)
  (let ((n (string->number s)))
    (unless (and n (exact? n) (integer? n) (positive? n))
      (error "dict: ~a 须为正整数: ~a" label s))
    n))

(define (parse-args args)
  (let lp ((rest args)
           (json? #f)
           (no-cache? #f)
           (refresh? #f)
           (clear? #f)
           (db #f)
           (help? #f)
           (color-pref 'auto)
           (sentence-force? #f)
           (rare? #f)
           (verbose? #f)
           (count-beside? #f)
           (top-n #f)
           (pos '()))
    (cond
      ((null? rest)
       (values json? no-cache? refresh? clear? db help? color-pref sentence-force? rare? verbose?
               count-beside? top-n (reverse pos)))
      ((or (string=? (car rest) "-h") (string=? (car rest) "--help"))
       (lp (cdr rest) json? no-cache? refresh? clear? db #t color-pref sentence-force? rare? verbose?
           count-beside? top-n pos))
      ((string=? (car rest) "--sentence")
       (lp (cdr rest) json? no-cache? refresh? clear? db help? color-pref #t rare? verbose?
           count-beside? top-n pos))
      ((string=? (car rest) "--rare")
       (lp (cdr rest) json? no-cache? refresh? clear? db help? color-pref sentence-force? #t verbose?
           count-beside? top-n pos))
      ((string=? (car rest) "--color")
       (lp (cdr rest) json? no-cache? refresh? clear? db help? 'always sentence-force? rare? verbose?
           count-beside? top-n pos))
      ((string=? (car rest) "--no-color")
       (lp (cdr rest) json? no-cache? refresh? clear? db help? 'never sentence-force? rare? verbose?
           count-beside? top-n pos))
      ((string=? (car rest) "--json")
       (lp (cdr rest) #t no-cache? refresh? clear? db help? color-pref sentence-force? rare? verbose?
           count-beside? top-n pos))
      ((or (string=? (car rest) "--verbose") (string=? (car rest) "-v"))
       (lp (cdr rest) json? no-cache? refresh? clear? db help? color-pref sentence-force? rare? #t
           count-beside? top-n pos))
      ((string=? (car rest) "--no-cache")
       (lp (cdr rest) json? #t refresh? clear? db help? color-pref sentence-force? rare? verbose?
           count-beside? top-n pos))
      ((string=? (car rest) "--refresh")
       (lp (cdr rest) json? no-cache? #t clear? db help? color-pref sentence-force? rare? verbose?
           count-beside? top-n pos))
      ((string=? (car rest) "--clear-cache")
       (lp (cdr rest) json? no-cache? refresh? #t db help? color-pref sentence-force? rare? verbose?
           count-beside? top-n pos))
      ((string=? (car rest) "--count-beside")
       (lp (cdr rest) json? no-cache? refresh? clear? db help? color-pref sentence-force? rare? verbose?
           #t top-n pos))
      ((string=? (car rest) "--top")
       (if (null? (cdr rest))
           (error "dict: --top 需要正整数参数")
           (lp (cddr rest) json? no-cache? refresh? clear? db help? color-pref sentence-force? rare?
               verbose? count-beside? (parse-positive-int (cadr rest) "--top") pos)))
      ((string-prefix? "--top=" (car rest))
       (let ((s (substring (car rest) (string-length "--top="))))
         (when (string-null? s)
           (error "dict: --top= 需要正整数"))
         (lp (cdr rest) json? no-cache? refresh? clear? db help? color-pref sentence-force? rare?
             verbose? count-beside? (parse-positive-int s "--top=") pos)))
      ((string=? (car rest) "--cache-db")
       (if (null? (cdr rest))
           (error "dict: --cache-db 需要路径参数")
           (lp (cddr rest) json? no-cache? refresh? clear? (cadr rest) help? color-pref sentence-force?
               rare? verbose? count-beside? top-n pos)))
      ((string-prefix? "--cache-db=" (car rest))
       (let ((p (substring (car rest) (string-length "--cache-db="))))
         (when (string-null? p)
           (error "dict: --cache-db= 需要路径"))
         (lp (cdr rest) json? no-cache? refresh? clear? p help? color-pref sentence-force? rare? verbose?
             count-beside? top-n pos)))
      ((string-prefix? "--" (car rest))
       (error "dict: 未知选项" (car rest)))
      (else (lp (cdr rest) json? no-cache? refresh? clear? db help? color-pref sentence-force? rare?
                verbose? count-beside? top-n (cons (car rest) pos))))))

(define (no-color-env?)
  (let ((v (libc-getenv "NO_COLOR")))
    (and v (not (string-null? v)))))

(define (use-terminal-color? color-pref)
  (cond ((eq? color-pref 'never) #f)
        ((eq? color-pref 'always) #t)
        (else (and (not (no-color-env?))
                   (libc-isatty (port->fdes (current-output-port)))))))

(define (nonempty-split-spaces s)
  (filter (lambda (x) (not (string-null? x))) (string-split s #\sp)))

(define (sentence-input? text force?)
  (or force?
      (let ((t (string-trim-both text)))
        (and (not (string-null? t))
             (or (>= (string-length t) 36)
                 (>= (length (nonempty-split-spaces t)) 2)
                 (string-index t #\,)
                 (string-index t #\.)
                 (string-index t #\;)
                 (string-index t #\:)
                 (string-index t #\?)
                 (string-index t #\!)
                 (string-index t #\xFF0C)
                 (string-index t #\x3002)
                 (string-index t #\xFF1F)
                 (string-index t #\xFF01))))))

(define (rare-words-as-list x)
  (cond ((vector? x) (vector->list x))
        ((list? x) x)
        (else '())))

(define (dedupe-rare-words words)
  (let lp ((ws words) (seen '()) (out '()))
    (cond ((null? ws) (reverse out))
          (else
           (let* ((w (string-trim-both (car ws)))
                  (k (normalize-key w)))
             (cond ((string-null? w) (lp (cdr ws) seen out))
                   ((< (string-length w) 2) (lp (cdr ws) seen out))
                   ((member k seen) (lp (cdr ws) seen out))
                   (else (lp (cdr ws) (cons k seen) (cons w out)))))))))

(define (alist-ref al key)
  (cond ((assoc key al) => cdr) (else #f)))

(define (fetch-deepseek-result-cached! db text no-cache? refresh? now rare?)
  (let* ((ck (deepseek-cache-key text rare?))
         (cached (and (not refresh?) (not no-cache?) (cache-get db ck))))
    (or (and cached
             (catch #t (lambda () (json-string->scm cached)) (lambda _ #f)))
        (receive (ok r msg) (deepseek-translate+rare/values text #:with-rare? rare?)
          (if ok
              (begin
                (cache-set! db ck
                            (scm->json-string
                             (if rare?
                                 (list (cons "translation" (alist-ref r "translation"))
                                       (cons "rare_words"
                                             (list->vector (alist-ref r "rare_words"))))
                                 (list (cons "translation" (alist-ref r "translation")))))
                            now)
                r)
              (begin
                (if (and msg (> (string-length msg) 0))
                    (format (current-error-port) "dict: DeepSeek：~a\n" msg)
                    (display "dict: DeepSeek 请求失败或返回无法解析\n"
                             (current-error-port)))
                #f))))))

(define (iciba-fetch-raw! db word no-cache? refresh? now verbose?)
  (let* ((nums (if verbose? 24 8))
         (key (string-append (normalize-key word) "\tsuggest-n" (number->string nums)))
         (cached (and (not refresh?) (not no-cache?) (cache-get db key)))
         (raw (or cached
                  (catch #t
                    (lambda () (iciba-fetch-suggest word #:nums nums))
                    (lambda (k . a)
                      (format (current-error-port) "请求失败: ~a ~a\n" k a)
                      #f)))))
    (when (and raw (not cached))
      (cache-set! db key raw now))
    raw))

(define (word-lookup-parts db word no-cache? refresh? now verbose?)
  (let ((raw (iciba-fetch-raw! db word no-cache? refresh? now verbose?)))
    (and raw
         (let ((data (catch #t (lambda () (json-string->scm raw)) (lambda _ #f))))
           (and data
                (let* ((entry (pick-best-entry word data))
                       (lk (and entry (cond ((assoc "key" entry) => cdr) (else #f))))
                       (en (and lk (fetch-enrichment db lk no-cache? refresh? now)))
                       (phonetics (if (dictapi-entries-vector? en)
                                      (dictapi-collect-phonetics en)
                                      '()))
                       (ex-cap (if verbose? 48 8))
                       (examples (if (dictapi-entries-vector? en)
                                     (dictapi-collect-examples en ex-cap)
                                     '()))
                       (cnt (lookup-count-bump! db (normalize-key word) now)))
                  (list data entry phonetics examples cnt)))))))

(define* (json-en-wrap phonetics examples)
  (if (and (null? phonetics) (null? examples))
      '()
      (list (cons "dictiloray_en"
                  (list (cons "phonetics" (list->vector phonetics))
                        (cons "examples" (list->vector examples)))))))

(define (print-word-lookup--json word parts)
  (and parts
       (match parts
         ((data entry ph ex cnt)
          (append (list (cons "query" word)
                        (cons "lookup_count" cnt)
                        (cons "iciba" data))
                  (json-en-wrap ph ex))))))

(define* (format-lookup-count-line cnt #:key (color? #f))
  (let ((s (format #f "查阅 ~a 次" cnt)))
    (if color?
        (string-append "\x1b[2m" s "\x1b[0m")
        s)))

(define* (format-query-head-with-count word cnt #:key (color? #f) (count-beside? #f))
  (if count-beside?
      (string-append word
                     (if color?
                         (string-append "\x1b[2m" (format #f " ×~a" cnt) "\x1b[0m")
                         (format #f " ×~a" cnt)))
      word))

(define (print-word-lookup--text word parts color? verbose? count-beside? db)
  (if (not parts)
      (format #t "~a: (请求失败)\n" word)
      (match parts
        ((data entry ph ex cnt)
         (if (not entry)
             (begin
               (format (current-error-port) "未解析到释义 (~a): ~a\n" word data)
               (format #t "~a: (无释义)\n"
                       (format-query-head-with-count word cnt #:color? color?
                                                      #:count-beside? count-beside?))
               (unless count-beside?
                 (display (format-lookup-count-line cnt #:color? color?))
                 (newline)))
             (begin
               (display (format-lookup-result word data
                                              #:color? color?
                                              #:phonetics ph
                                              #:examples ex
                                              #:verbose? verbose?
                                              #:lookup-count-beside? count-beside?
                                              #:lookup-count (if count-beside? cnt #f)
                                              #:lookup-count-get
                                              (if (and count-beside? verbose?)
                                                  (lambda (k)
                                                    (if (string? k) (lookup-count-get db k) 0))
                                                  #f)))
               (newline)
               (unless count-beside?
                 (display (format-lookup-count-line cnt #:color? color?))
                 (newline))
               (newline)))))))

(define* (print-word-lookup word db no-cache? refresh? now color? json? verbose?
          count-beside?)
  (let ((parts (word-lookup-parts db word no-cache? refresh? now verbose?)))
    (if json?
        (print-word-lookup--json word parts)
        (print-word-lookup--text word parts color? verbose? count-beside? db))))

(define (run-sentence-mode db text json? no-cache? refresh? color-pref now rare? verbose?
          count-beside?)
  (let ((color? (use-terminal-color? color-pref))
        (key-ok? (deepseek-api-key-from-env)))
    (unless key-ok?
      (display "dict: 句子翻译需要设置环境变量 DEEPSEEK_API_KEY\n" (current-error-port))
      (primitive-exit 1))
    (let ((ds (fetch-deepseek-result-cached! db text no-cache? refresh? now rare?)))
      (unless ds
        (primitive-exit 1))
      (let* ((tr (alist-ref ds "translation"))
             (rws (if rare?
                      (dedupe-rare-words (rare-words-as-list (alist-ref ds "rare_words")))
                      '())))
        (if json?
            (let* ((word-objs
                    (if rare?
                        (map (lambda (w)
                               (or (print-word-lookup w db no-cache? refresh? now color? #t verbose?
                                                     count-beside?)
                                   (list (cons "query" w)
                                         (cons "iciba" '())
                                         (cons "error" "lookup_failed"))))
                             rws)
                        '()))
                   (payload0
                    (list (cons "mode" "sentence")
                          (cons "sentence" text)
                          (cons "translation" tr)))
                   (payload
                    (if rare?
                        (append payload0
                                (list (cons "rare_words" (list->vector rws))
                                      (cons "words" (list->vector word-objs))))
                        payload0)))
              (display (scm->json-pretty-string payload)))
            (begin
              (display (format-section-banner "译文" #:color? color?))
              (newline)
              (display tr)
              (newline)
              (when rare?
                (newline)
                (display (format-section-banner "生僻词" #:color? color?))
                (newline)
                (if (null? rws)
                    (display "（无）\n")
                    (begin
                      (display (string-join rws " · "))
                      (newline)))
                (newline)
                (for-each
                 (lambda (w)
                   (print-word-lookup w db no-cache? refresh? now color? #f verbose?
                                     count-beside?))
                 rws))))))))

(define (run-word-mode--json word data ph ex cnt verbose?)
  (let* ((payload0 (json-with-en data ph ex))
         (payload1 (cons (cons "lookup_count" cnt) payload0))
         (out (if verbose? (cons (cons "query" word) payload1) payload1)))
    (display (scm->json-pretty-string out))))

(define (run-word-mode--text word data entry ph ex cnt color? verbose? count-beside? db)
  (unless entry
    (format (current-error-port) "未解析到释义: ~a\n" data)
    (primitive-exit 1))
  (display (format-lookup-result word data
                                 #:color? color?
                                 #:phonetics ph
                                 #:examples ex
                                 #:verbose? verbose?
                                 #:lookup-count-beside? count-beside?
                                 #:lookup-count (if count-beside? cnt #f)
                                 #:lookup-count-get
                                 (if (and count-beside? verbose?)
                                     (lambda (k) (if (string? k) (lookup-count-get db k) 0))
                                     #f)))
  (newline)
  (unless count-beside?
    (display (format-lookup-count-line cnt #:color? color?))
    (newline)))

(define (run-top-stats db n json? color-pref)
  (let ((rows (lookup-stats-top db n))
        (color? (use-terminal-color? color-pref)))
    (if json?
        (display
         (scm->json-pretty-string
          (list (cons "top"
                        (list->vector
                         (map (lambda (p)
                                (list (cons "word" (car p)) (cons "count" (cdr p))))
                              rows))))))
        (begin
          (display (format-section-banner (format #f "高频查词（前 ~a）" n) #:color? color?))
          (newline)
          (if (null? rows)
              (display "（暂无记录）\n")
              (for-each
               (lambda (p) (format #t "  ~8a  ~a\n" (cdr p) (car p)))
               rows))))))

(define (run-word-mode db word json? no-cache? refresh? color-pref now verbose? count-beside?)
  (let ((color? (use-terminal-color? color-pref))
        (parts (word-lookup-parts db word no-cache? refresh? now verbose?)))
    (unless parts
      (primitive-exit 1))
    (match parts
      ((data entry ph ex cnt)
       (if json?
           (run-word-mode--json word data ph ex cnt verbose?)
           (run-word-mode--text word data entry ph ex cnt color? verbose? count-beside? db))))))

(define (main)
  (receive (json? no-cache? refresh? clear? db-path help? color-pref sentence-force? rare? verbose?
            count-beside? top-n pos)
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
      (when (and top-n (pair? pos))
        (display "dict: --top 与查词不能同时使用\n" (current-error-port))
        (primitive-exit 2))
      (when top-n
        (ensure-cache-parent-dir db)
        (run-top-stats db top-n json? color-pref)
        (primitive-exit 0))
      (when (null? pos)
        (usage)
        (primitive-exit 2))
      (when (not (null? (cdr pos)))
        (display "dict: 请只传一段文本（句子请加引号）\n" (current-error-port))
        (primitive-exit 2))
      (let* ((text (car pos))
             (now (now-epoch)))
        (ensure-cache-parent-dir db)
        (if (sentence-input? text sentence-force?)
            (run-sentence-mode db text json? no-cache? refresh? color-pref now rare? verbose?
                               count-beside?)
            (run-word-mode db text json? no-cache? refresh? color-pref now verbose?
                           count-beside?))))))

(main)
