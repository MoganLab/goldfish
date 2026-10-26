(import (liii check)
  (liii pi code core extensions)
) ;import

(check-set-mode! 'report-failed)

(define (get-fixtures-dir)
  (cond
    ((file-exists? "tests/fixtures/extensions") "tests/fixtures/extensions")
    ((file-exists? "tools/pi/tests/fixtures/extensions") "tools/pi/tests/fixtures/extensions")
    (else "../../tools/pi/tests/fixtures/extensions")
  ) ;cond
) ;define

;; 1. 加载合法的单文件扩展
(let* ((api (make-extension-api))
       (path (string-append (get-fixtures-dir) "/valid-ext1.scm"))
       (res (pi-load-extension path api)))
  (check (pair? res) => #t)
  (check (eq? (cdr (assq 'status res)) 'ok) => #t)
  (check (equal? (cdr (assq 'value res)) "ext1-loaded") => #t)
  ;; 验证扩展在 api 中成功注册了 test-tool-1
  (let ((tool (find-extension-tool api "test-tool-1")))
    (check (extension-tool-def? tool) => #t)
    (check (string=? (extension-tool-def-name tool) "test-tool-1") => #t)
  ) ;let
) ;let*

;; 2. 加载包含 index.scm 的目录扩展
(let* ((api (make-extension-api))
       (path (string-append (get-fixtures-dir) "/valid-pkg/index.scm"))
       (res (pi-load-extension path api)))
  (check (pair? res) => #t)
  (check (eq? (cdr (assq 'status res)) 'ok) => #t)
  (check (equal? (cdr (assq 'value res)) "pkg-loaded") => #t)
  ;; 验证在 api 中成功注册了 --pkg-flag
  (let ((flag (find-extension-flag api "--pkg-flag")))
    (check (extension-flag-def? flag) => #t)
    (check (string=? (extension-flag-def-flag flag) "--pkg-flag") => #t)
  ) ;let
) ;let*

;; 3. 加载非过程返回值的扩展文件，安全返回 ok
(let* ((api (make-extension-api))
       (path (string-append (get-fixtures-dir) "/non-proc.scm"))
       (res (pi-load-extension path api)))
  (check (pair? res) => #t)
  (check (eq? (cdr (assq 'status res)) 'ok) => #t)
  (check (equal? (cdr (assq 'value res)) "not-a-procedure") => #t)
) ;let*

;; 4. 加载不存在的文件，捕获错误返回 status: error，不崩溃
(let* ((api (make-extension-api))
       (path (string-append (get-fixtures-dir) "/non-existent-xyz.scm"))
       (res (pi-load-extension path api)))
  (check (pair? res) => #t)
  (check (eq? (cdr (assq 'status res)) 'error) => #t)
  (check (string? (cdr (assq 'error res))) => #t)
) ;let*

;; 5. 加载语法错误的文件，捕获异常返回 status: error，不崩溃
(let* ((api (make-extension-api))
       (path (string-append (get-fixtures-dir) "/syntax-error.scm"))
       (res (pi-load-extension path api)))
  (check (pair? res) => #t)
  (check (eq? (cdr (assq 'status res)) 'error) => #t)
) ;let*

;; 6. 执行时抛错的扩展，捕获异常返回 status: error，不崩溃
(let* ((api (make-extension-api))
       (path (string-append (get-fixtures-dir) "/runtime-error.scm"))
       (res (pi-load-extension path api)))
  (check (pair? res) => #t)
  (check (eq? (cdr (assq 'status res)) 'error) => #t)
) ;let*

;; 7. 传递 ExtensionContext 参数
(let* ((api (make-extension-api))
       (ctx (make-extension-context :session-id "s-123" :registry (extension-api-registry api)))
       (path (string-append (get-fixtures-dir) "/valid-ext1.scm"))
       (res (pi-load-extension path api ctx)))
  (check (eq? (cdr (assq 'status res)) 'ok) => #t)
) ;let*

(check-report)
