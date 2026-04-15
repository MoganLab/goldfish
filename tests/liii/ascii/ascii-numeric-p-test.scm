(import (liii check) (liii ascii))


;; ascii-numeric?
;; 判断是否为 ASCII 数字。
;;
;; 语法
;; ----
;; (ascii-numeric? x)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要判断的字符或码点。
;;
;; 返回值
;; ----
;; boolean
;; 如果x是 ASCII 数字字符则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 覆盖 0-9 的数字边界。
;;
;; 示例
;; ----
;; (ascii-numeric? #\0) => #t
;; (ascii-numeric? #\a) => #f
;;
;; 错误处理
;; ----
;; 类型或范围不匹配时返回 #f


(check-true (ascii-numeric? #\0))
(check-true (ascii-numeric? #\9))
(check-false (ascii-numeric? #\a))


(check-report)
