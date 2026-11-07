;; pi-arg-ref
;; 从工具参数列表中按符号或字符串键读取参数
;;
;; 语法
;; ----
;; (pi-arg-ref ...)
;;
;; 参数
;; ----
;; 见各断言场景：本测试按用例组织，覆盖典型入参。
;;
;; 返回值
;; ------
;; 见各断言场景：按被测函数实际返回值断言。

(import (liii check) (liii pi code core tools read))

(check-set-mode! 'report-failed)

;; pi-arg-ref
;; 从工具参数列表中按符号或字符串键读取参数。
(let ((args '(("path" . "/tmp/foo") (limit . 10))))
  (check (pi-arg-ref args 'path) => "/tmp/foo")
  (check (pi-arg-ref args "limit") => 10)
  (check (pi-arg-ref args 'offset #f) => #f)
) ;let

(check-report)
