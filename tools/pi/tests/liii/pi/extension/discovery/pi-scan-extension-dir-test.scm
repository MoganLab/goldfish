(import (liii check)
  (liii path)
  (liii os)
  (liii list)
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

;; 1. 扫描不存在的目录安全返回空列表
(check (pi-scan-extension-dir "/non/existent/path/for/sure/12345") => '())

;; 2. 扫描非目录文件安全返回空列表
(check (pi-scan-extension-dir (string-append (get-fixtures-dir) "/ignore.txt")) => '())

;; 3. 扫描测试 fixtures 目录：
;; 包含 valid-ext1.scm, valid-pkg/index.scm, non-proc.scm, syntax-error.scm, runtime-error.scm
;; 排除 ignore.txt (.txt 不是 .scm), .hidden.scm (以 . 开头), dir-without-index (无 index.scm)
(let* ((fixture-dir (get-fixtures-dir))
       (found (pi-scan-extension-dir fixture-dir)))
  (check (list? found) => #t)
  ;; 验证发现的项目都是 .scm 结尾
  (for-each
    (lambda (p)
      (check (string? p) => #t)
      (check (string=? (path-suffix (path p)) ".scm") => #t)
    ) ;lambda
    found
  ) ;for-each

  ;; 验证包含 valid-ext1.scm
  (check (pair? (filter (lambda (p) (string=? (path-name (path p)) "valid-ext1.scm")) found)) => #t)

  ;; 验证包含 valid-pkg/index.scm
  (check (pair? (filter (lambda (p) (and (string=? (path-name (path p)) "index.scm")
                                         (string=? (path-name (path-parent (path p))) "valid-pkg")))
                        found)) => #t)

  ;; 验证排除了 ignore.txt
  (check (null? (filter (lambda (p) (string=? (path-name (path p)) "ignore.txt")) found)) => #t)

  ;; 验证排除了 .hidden.scm
  (check (null? (filter (lambda (p) (string=? (path-name (path p)) ".hidden.scm")) found)) => #t)

  ;; 验证排除了 dir-without-index
  (check (null? (filter (lambda (p) (string=? (path-name (path-parent (path p))) "dir-without-index")) found)) => #t)
) ;let*

;; 4. 扫描空目录返回空列表
(let ((empty-dir (string-append (get-fixtures-dir) "/empty-temp-dir")))
  (catch #t (lambda () (mkdir empty-dir)) (lambda args #f))
  (check (pi-scan-extension-dir empty-dir) => '())
  (catch #t (lambda () (rmdir empty-dir)) (lambda args #f))
) ;let

(check-report)
