(import (liii check)
        (liii ascii)
) ;import

;; ascii-left-paren?
;; 判断是否为 ASCII 左括号。
;;
;; 语法
;; ----
;; (ascii-left-paren? x)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要判断的字符或码点。
;;
;; 返回值
;; ----
;; boolean
;; 如果x是左括号字符`(`或对应码点则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 支持字符和整数码点两种输入。
;;
;; 示例
;; ----
;; (ascii-left-paren? #\() => #t
;; (ascii-left-paren? #x29) => #f
;;
;; 错误处理
;; ----
;; 类型或范围不匹配时返回 #f

(check-true (ascii-left-paren? #\())
(check-true (ascii-left-paren? #x28))
(check-false (ascii-left-paren? #\)))
(check-false (ascii-left-paren? #x29))
(check-false (ascii-left-paren? #\A))
(check-false (ascii-left-paren? #\[))

(check-report)
