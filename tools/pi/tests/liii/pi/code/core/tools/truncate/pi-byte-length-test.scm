;; pi-byte-length
;; 计算字符串的 UTF-8 字节长度，用于工具输出截断配额
;;
;; 语法
;; ----
;; (pi-byte-length ...)
;;
;; 参数
;; ----
;; 见各断言场景：本测试按用例组织，覆盖典型入参。
;;
;; 返回值
;; ------
;; 见各断言场景：按被测函数实际返回值断言。

(import (liii check) (liii pi code core tools truncate))

(check-set-mode! 'report-failed)

;; pi-byte-length
;; 计算字符串的 UTF-8 字节长度。
(check (pi-byte-length "hello") => 5)
(check (pi-byte-length "金鱼") => 6)

(check-report)
