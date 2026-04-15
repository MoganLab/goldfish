(import (liii check) (liii ascii))


;; ascii-whitespace?
;; 判断是否为 ASCII 空白字符。
;;
;; 语法
;; ----
;; (ascii-whitespace? x)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要判断的字符或码点。
;;
;; 返回值
;; ----
;; boolean
;; 如果x是 ASCII 空白字符则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 覆盖空格、制表符、换行等常见空白字符。
;;
;; 示例
;; ----
;; (ascii-whitespace? #\space) => #t
;; (ascii-whitespace? #\A) => #f
;;
;; 错误处理
;; ----
;; 类型或范围不匹配时返回 #f


(check-true (ascii-whitespace? #\tab))
(check-true (ascii-whitespace? #\newline)
) ;check-true
(check-true (ascii-whitespace? #\space))
(check-false (ascii-whitespace? #\A))


(check-report)
