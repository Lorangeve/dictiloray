;;; SQLite cache via Guile FFI (requires libsqlite3 at runtime).

(define-module (dictiloray cache)
  #:use-module (dictiloray libc-mini)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-13)
  #:use-module (system foreign)
  #:export (cache-get
            cache-set!
            cache-clear!
            ensure-cache-parent-dir))

(define SQLITE-OK 0)
(define SQLITE-ROW 100)
(define SQLITE-DONE 101)

(define %lib #f)
(define %sqlite3-open #f)
(define %sqlite3-close #f)
(define %sqlite3-prepare-v2 #f)
(define %sqlite3-bind-text #f)
(define %sqlite3-bind-int64 #f)
(define %sqlite3-step #f)
(define %sqlite3-column-text #f)
(define %sqlite3-finalize #f)
(define %sqlite3-changes #f)
(define %sqlite3-exec #f)
(define %sqlite3-free #f)

;; SQLITE_TRANSIENT = (void(*)(void*))-1 — Guile 无 integer->pointer 时用全 1 地址。
(define SQLITE-TRANSIENT
  (make-pointer (- (expt 2 (* 8 (sizeof '*))) 1)))

;; Guile 3.0.9 (rnrs bytevectors) has no bytevector-pointer-ref.
(define (bytevector-native-pointer-ref bv offset)
  (make-pointer
   (case (sizeof '*)
     ((4) (bytevector-u32-native-ref bv offset))
     ((8) (bytevector-u64-native-ref bv offset))
     (else (error "dictiloray cache: unsupported pointer size")))))

(define (%sqlite-library-candidates)
  "Short sonames work on typical distros; absolute paths help Guix Guile (narrow rpath).
Optional override: DICTILORAY_SQLITE3=/path/to/libsqlite3.so.0"
  (append
   (let ((e (libc-getenv "DICTILORAY_SQLITE3")))
     (if (and e (not (string-null? e))) (list e) '()))
   '("libsqlite3.so.0" "libsqlite3.so" "libsqlite3"
     "/usr/lib/libsqlite3.so.0"
     "/usr/lib64/libsqlite3.so.0"
     "/usr/lib/x86_64-linux-gnu/libsqlite3.so.0")))

(define (sqlite-init!)
  (unless %lib
    (set! %lib
          (let try ((names (%sqlite-library-candidates)))
            (cond
              ((null? names)
               (error
                "dictiloray: cannot dynamic-link libsqlite3; install SQLite or set DICTILORAY_SQLITE3 to the .so path"))
              (else
               (catch #t
                 (lambda () (dynamic-link (car names)))
                 (lambda _ (try (cdr names))))))))
    (set! %sqlite3-open
          (pointer->procedure int
            (dynamic-func "sqlite3_open" %lib)
            (list '* '*)))
    (set! %sqlite3-close
          (pointer->procedure int
            (dynamic-func "sqlite3_close" %lib)
            (list '*)))
    (set! %sqlite3-prepare-v2
          (pointer->procedure int
            (dynamic-func "sqlite3_prepare_v2" %lib)
            (list '* '* int '* '*)))
    (set! %sqlite3-bind-text
          (pointer->procedure int
            (dynamic-func "sqlite3_bind_text" %lib)
            (list '* int '* int '*)))
    (set! %sqlite3-bind-int64
          (pointer->procedure int
            (dynamic-func "sqlite3_bind_int64" %lib)
            (list '* int int64)))
    (set! %sqlite3-step
          (pointer->procedure int
            (dynamic-func "sqlite3_step" %lib)
            (list '*)))
    (set! %sqlite3-column-text
          (pointer->procedure '*
            (dynamic-func "sqlite3_column_text" %lib)
            (list '* int)))
    (set! %sqlite3-finalize
          (pointer->procedure int
            (dynamic-func "sqlite3_finalize" %lib)
            (list '*)))
    (set! %sqlite3-changes
          (pointer->procedure int
            (dynamic-func "sqlite3_changes" %lib)
            (list '*)))
    (set! %sqlite3-exec
          (pointer->procedure int
            (dynamic-func "sqlite3_exec" %lib)
            (list '* '* '* '* '*)))
    (set! %sqlite3-free
          (pointer->procedure void
            (dynamic-func "sqlite3_free" %lib)
            (list '*)))))

(define (open-db path)
  (sqlite-init!)
  (let* ((path-ptr (string->pointer path))
         (out (make-bytevector (sizeof '*) 0))
         (rc (%sqlite3-open path-ptr (bytevector->pointer out))))
    (when (not (zero? rc))
      (error "sqlite3_open failed" rc path))
    (let ((db (bytevector-native-pointer-ref out 0)))
      (when (null-pointer? db)
        (error "sqlite3_open returned null db" path))
      db)))

(define (close-db db)
  (unless (null-pointer? db)
    (let ((rc (%sqlite3-close db)))
      (unless (zero? rc)
        (error "sqlite3_close failed" rc)))))

(define (exec-ddl db sql)
  (let ((err-bv (make-bytevector (sizeof '*) 0))
        (sql-ptr (string->pointer sql)))
    (let ((rc (%sqlite3-exec db sql-ptr %null-pointer %null-pointer
                             (bytevector->pointer err-bv))))
      (let ((errptr (bytevector-native-pointer-ref err-bv 0)))
        (unless (null-pointer? errptr)
          (%sqlite3-free errptr)))
      (unless (zero? rc)
        (error "sqlite3_exec failed" rc sql)))))

(define (with-db path proc)
  (let ((db (open-db path)))
    (dynamic-wind
      (lambda () #t)
      (lambda () (proc db))
      (lambda () (close-db db)))))

(define (ensure-cache-parent-dir path)
  (define (dirname* p)
    (let ((i (string-rindex p #\/)))
      (cond ((not i) ".")
            ((zero? i) "/")
            (else (substring p 0 i)))))
  (define (mkdir-p dir)
    (unless (or (string=? dir ".") (libc-file-exists? dir))
      (let ((parent (dirname* dir)))
        (unless (or (string=? parent dir) (string=? parent "."))
          (mkdir-p parent))
        (unless (libc-mkdir dir #o755)
          (error "dictiloray: mkdir failed" dir)))))
  (mkdir-p (dirname* path)))

(define (ensure-schema db)
  (exec-ddl db
            (string-append
              "CREATE TABLE IF NOT EXISTS entries ("
              "query_key TEXT PRIMARY KEY,"
              "payload TEXT NOT NULL,"
              "fetched_at INTEGER NOT NULL)")))

(define (cache-get path query-key)
  (with-db path
    (lambda (db)
      (ensure-schema db)
      (let* ((sql "SELECT payload FROM entries WHERE query_key = ?1")
             (stmt-bv (make-bytevector (sizeof '*) 0))
             (rc (%sqlite3-prepare-v2 db (string->pointer sql) -1
                                      (bytevector->pointer stmt-bv)
                                      %null-pointer)))
        (unless (zero? rc)
          (error "prepare SELECT failed" rc))
        (let ((stmt (bytevector-native-pointer-ref stmt-bv 0)))
          (dynamic-wind
            (lambda () #t)
            (lambda ()
              (let ((b (%sqlite3-bind-text stmt 1 (string->pointer query-key)
                                           -1 SQLITE-TRANSIENT)))
                (unless (zero? b)
                  (error "bind failed" b)))
              (let ((s (%sqlite3-step stmt)))
                (cond
                  ((= s SQLITE-ROW)
                   (let ((col (%sqlite3-column-text stmt 0)))
                     (if (null-pointer? col)
                         #f
                         (pointer->string col))))
                  ((= s SQLITE-DONE) #f)
                  (else (error "step SELECT failed" s)))))
            (lambda ()
              (%sqlite3-finalize stmt))))))))

(define (cache-set! path query-key payload-str fetched-at)
  (with-db path
    (lambda (db)
      (ensure-schema db)
      (let* ((sql (string-append
                    "INSERT INTO entries (query_key, payload, fetched_at) "
                    "VALUES (?1, ?2, ?3) "
                    "ON CONFLICT(query_key) DO UPDATE SET "
                    "payload = excluded.payload, "
                    "fetched_at = excluded.fetched_at"))
             (stmt-bv (make-bytevector (sizeof '*) 0))
             (rc (%sqlite3-prepare-v2 db (string->pointer sql) -1
                                      (bytevector->pointer stmt-bv)
                                      %null-pointer)))
        (unless (zero? rc)
          (error "prepare INSERT failed" rc))
        (let ((stmt (bytevector-native-pointer-ref stmt-bv 0)))
          (dynamic-wind
            (lambda () #t)
            (lambda ()
              (unless (zero? (%sqlite3-bind-text stmt 1 (string->pointer query-key)
                                                   -1 SQLITE-TRANSIENT))
                (error "bind key failed"))
              (unless (zero? (%sqlite3-bind-text stmt 2 (string->pointer payload-str)
                                                   -1 SQLITE-TRANSIENT))
                (error "bind payload failed"))
              (unless (zero? (%sqlite3-bind-int64 stmt 3 fetched-at))
                (error "bind time failed"))
              (let ((s (%sqlite3-step stmt)))
                (unless (= s SQLITE-DONE)
                  (error "step INSERT failed" s))))
            (lambda ()
              (%sqlite3-finalize stmt))))))))

(define (cache-clear! path)
  (with-db path
    (lambda (db)
      (ensure-schema db)
      (exec-ddl db "DELETE FROM entries")
      (%sqlite3-changes db))))
