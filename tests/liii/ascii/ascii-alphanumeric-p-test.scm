(import (liii check)
        (liii ascii)
) ;import

;; ascii-alphanumeric?
;; 判断是否为 ASCII 字母或数字。
;;
;; 语法
;; ----
;; (ascii-alphanumeric? x)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要判断的字符或码点。
;;
;; 返回值
;; ----
;; boolean
;; 如果x是 ASCII 字母或数字则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 常用于标识符类字符的判断。
;;
;; 示例
;; ----
;; (ascii-alphanumeric? #\0) => #t
;; (ascii-alphanumeric? #\-) => #f
;;
;; 错误处理
;; ----
;; 类型或范围不匹配时返回 #f

(check-true (ascii-alphanumeric? #\0))
(check-true (ascii-alphanumeric? #\G))
(check-false (ascii-alphanumeric? #\-))

(check-report)
