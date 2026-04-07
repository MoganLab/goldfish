(import (liii check)
        (liii ascii)
) ;import

;; ascii-non-control?
;; 判断是否为 ASCII 非控制字符。
;;
;; 语法
;; ----
;; (ascii-non-control? x)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要判断的字符或码点。
;;
;; 返回值
;; ----
;; boolean
;; 如果x是 ASCII 可打印区间字符则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 覆盖 #x20 到 #x7e 的边界。
;;
;; 示例
;; ----
;; (ascii-non-control? #x20) => #t
;; (ascii-non-control? #x7f) => #f
;;
;; 错误处理
;; ----
;; 类型或范围不匹配时返回 #f

(check-true (ascii-non-control? #x20))
(check-true (ascii-non-control? #x7e))
(check-false (ascii-non-control? #x1f))
(check-false (ascii-non-control? #x7f))

(check-report)
