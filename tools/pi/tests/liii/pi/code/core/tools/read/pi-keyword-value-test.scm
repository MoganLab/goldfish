;; pi-keyword-value
;; 从工具参数列表中按 :keyword 提取值
;;
;; 语法
;; ----
;; (pi-keyword-value ...)
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

;; pi-keyword-value
;; 从函数参数列表中按关键字读取对应的值。
(check (pi-keyword-value :timeout '(:timeout 30 :cwd "/tmp") 10) => 30)
(check (pi-keyword-value :timeout '(:cwd "/tmp") 10) => 10)

(check-report)
