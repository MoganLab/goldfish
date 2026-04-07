(import (liii check)
        (liii ascii)
) ;import

;; ascii-control?
;; 判断是否为 ASCII 控制字符。
;;
;; 语法
;; ----
;; (ascii-control? x)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要判断的字符或码点。
;;
;; 返回值
;; ----
;; boolean
;; 如果x位于 ASCII 控制字符范围内则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 覆盖 #x00-#x1f 以及 #x7f 边界。
;;
;; 示例
;; ----
;; (ascii-control? 0) => #t
;; (ascii-control? #x20) => #f
;;
;; 错误处理
;; ----
;; 类型或范围不匹配时返回 #f

(check-true (ascii-control? 0))
(check-true (ascii-control? #x1f))
(check-true (ascii-control? #x7f))
(check-false (ascii-control? #x20))

(check-report)
