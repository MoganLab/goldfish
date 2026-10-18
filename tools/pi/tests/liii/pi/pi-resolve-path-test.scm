(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-resolve-path
;; 将相对路径或带 ~ 的路径解析为规范的绝对路径。
(let ((p (pi-resolve-path "PI_PLAN.md" "/tmp")))
  (check-true (string? p))
  (check (char=? (string-ref p 0) #\/) => #t))

(check-report)
