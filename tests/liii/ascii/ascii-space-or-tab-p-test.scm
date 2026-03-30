(import (liii check)
        (liii ascii))

;; ascii-space-or-tab?
;; 判断是否为空格或制表符。
;;
;; 语法
;; ----
;; (ascii-space-or-tab? x)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要判断的字符或码点。
;;
;; 返回值
;; ----
;; boolean
;; 如果x是空格或制表符则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 与ascii-whitespace?不同，不包含换行符。
;;
;; 示例
;; ----
;; (ascii-space-or-tab? #\space) => #t
;; (ascii-space-or-tab? #\newline) => #f
;;
;; 错误处理
;; ----
;; 类型或范围不匹配时返回 #f

(check-true (ascii-space-or-tab? #\space))
(check-true (ascii-space-or-tab? #\tab))
(check-false (ascii-space-or-tab? #\newline))

(check-report)
