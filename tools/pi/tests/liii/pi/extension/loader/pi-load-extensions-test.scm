(import (liii check)
  (liii pi extension)
) ;import

(check-set-mode! 'report-failed)

(define (get-fixtures-dir)
  (cond
    ((file-exists? "tests/fixtures/extensions") "tests/fixtures/extensions")
    ((file-exists? "tools/pi/tests/fixtures/extensions") "tools/pi/tests/fixtures/extensions")
    (else "../../tools/pi/tests/fixtures/extensions")
  ) ;cond
) ;define

;; 1. 批量加载空列表
(let* ((api (make-extension-api))
       (report (pi-load-extensions '() api)))
  (check (pair? report) => #t)
  (check (cdr (assq 'total report)) => 0)
  (check (cdr (assq 'success report)) => 0)
  (check (cdr (assq 'failed report)) => 0)
  (check (pi-load-results-success-count report) => 0)
  (check (pi-load-results-failed-count report) => 0)
) ;let*

;; 2. 批量加载两个合法扩展
(let* ((api (make-extension-api))
       (paths (list (string-append (get-fixtures-dir) "/valid-ext1.scm")
                    (string-append (get-fixtures-dir) "/valid-pkg/index.scm")))
       (report (pi-load-extensions paths api)))
  (check (cdr (assq 'total report)) => 2)
  (check (cdr (assq 'success report)) => 2)
  (check (cdr (assq 'failed report)) => 0)
  (check (pi-load-results-success-count report) => 2)
  (check (pi-load-results-failed-count report) => 0)
  ;; 工具和 Flag 均正常注册
  (check (extension-tool-def? (find-extension-tool api "test-tool-1")) => #t)
  (check (extension-flag-def? (find-extension-flag api "--pkg-flag")) => #t)
) ;let*

;; 3. 混合加载：1 个合法，3 个故障（不存在、语法错误、运行时错误）
(let* ((api (make-extension-api))
       (paths (list (string-append (get-fixtures-dir) "/valid-ext1.scm")
                    (string-append (get-fixtures-dir) "/syntax-error.scm")
                    (string-append (get-fixtures-dir) "/runtime-error.scm")
                    (string-append (get-fixtures-dir) "/not-exist.scm")))
       (report (pi-load-extensions paths api)))
  (check (cdr (assq 'total report)) => 4)
  (check (cdr (assq 'success report)) => 1)
  (check (cdr (assq 'failed report)) => 3)
  (check (pi-load-results-success-count report) => 1)
  (check (pi-load-results-failed-count report) => 3)
  ;; 合法工具依然成功注册，不受故障扩展影响
  (check (extension-tool-def? (find-extension-tool api "test-tool-1")) => #t)
) ;let*

(check-report)
