(import (liii check)
        (liii ascii)
) ;import

;; ascii-other-graphic?
;; 判断是否为可见的非字母数字 ASCII 图形字符。
;;
;; 语法
;; ----
;; (ascii-other-graphic? x)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要判断的字符或码点。
;;
;; 返回值
;; ----
;; boolean
;; 如果x是标点等可见非字母数字字符则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 常用于识别标点和分隔符。
;;
;; 示例
;; ----
;; (ascii-other-graphic? #\!) => #t
;; (ascii-other-graphic? #\A) => #f
;;
;; 错误处理
;; ----
;; 类型或范围不匹配时返回 #f

(check-true (ascii-other-graphic? #\!))
(check-true (ascii-other-graphic? #\{))
(check-false (ascii-other-graphic? #\A))
(check-false (ascii-other-graphic? #\0))

(check-report)
