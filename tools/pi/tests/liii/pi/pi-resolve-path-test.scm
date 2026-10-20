(import (liii check) (liii path) (liii pi))

(check-set-mode! 'report-failed)

;; pi-resolve-path
;; 将相对路径或带 ~ 的路径解析为规范的绝对路径。
(let ((p (pi-resolve-path "tools/pi/tests/fixtures/sample.txt" (path->string (path-temp-dir)))))
  (check-true (string? p))
  (check-true (path-absolute? (path p))))

(check-report)
