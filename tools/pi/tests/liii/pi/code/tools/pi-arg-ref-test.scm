(import (liii check) (liii pi code tools read))

(check-set-mode! 'report-failed)

;; pi-arg-ref
;; 从工具参数列表中按符号或字符串键读取参数。
(let ((args '(("path" . "/tmp/foo") (limit . 10))))
  (check (pi-arg-ref args 'path) => "/tmp/foo")
  (check (pi-arg-ref args "limit") => 10)
  (check (pi-arg-ref args 'offset #f) => #f))

(check-report)
