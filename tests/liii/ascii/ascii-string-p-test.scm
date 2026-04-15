(import (liii check) (liii ascii))


;; ascii-string?
;; 判断字符串是否全部由 ASCII 字符组成。
;;
;; 语法
;; ----
;; (ascii-string? x)
;;
;; 参数
;; ----
;; x : any?
;; 要判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 如果x是只包含 ASCII 字符的字符串则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 空字符串也视为 ASCII 字符串。
;;
;; 示例
;; ----
;; (ascii-string? "Goldfish") => #t
;; (ascii-string? "G中") => #f
;;
;; 错误处理
;; ----
;; 非字符串输入返回 #f


(check-true (ascii-string? "Goldfish"))
(check-true (ascii-string? "A\tB\nC"))
(check-false (ascii-string? "G中"))
(check-false (ascii-string? #\A))


(check-report)
