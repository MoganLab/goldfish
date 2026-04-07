(import (liii check)
        (liii ascii)
) ;import

;; ascii-alphabetic?
;; 判断是否为 ASCII 字母。
;;
;; 语法
;; ----
;; (ascii-alphabetic? x)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要判断的字符或码点。
;;
;; 返回值
;; ----
;; boolean
;; 如果x是 ASCII 字母则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 同时覆盖大小写字母。
;;
;; 示例
;; ----
;; (ascii-alphabetic? #\A) => #t
;; (ascii-alphabetic? #\0) => #f
;;
;; 错误处理
;; ----
;; 类型或范围不匹配时返回 #f

(check-true (ascii-alphabetic? #\A))
(check-true (ascii-alphabetic? #\z))
(check-false (ascii-alphabetic? #\0))

(check-report)
