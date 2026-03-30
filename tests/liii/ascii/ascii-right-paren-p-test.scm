(import (liii check)
        (liii ascii))

;; ascii-right-paren?
;; 判断是否为 ASCII 右括号。
;;
;; 语法
;; ----
;; (ascii-right-paren? x)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要判断的字符或码点。
;;
;; 返回值
;; ----
;; boolean
;; 如果x是右括号字符`)`或对应码点则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 支持字符和整数码点两种输入。
;;
;; 示例
;; ----
;; (ascii-right-paren? #\)) => #t
;; (ascii-right-paren? #x28) => #f
;;
;; 错误处理
;; ----
;; 类型或范围不匹配时返回 #f

(check-true (ascii-right-paren? #\)))
(check-true (ascii-right-paren? #x29))
(check-false (ascii-right-paren? #\())
(check-false (ascii-right-paren? #x28))
(check-false (ascii-right-paren? #\A))
(check-false (ascii-right-paren? #\]))

(check-report)
