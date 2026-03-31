(import (liii check)
        (liii range)
) ;import

(check-set-mode! 'report-failed)

;; range?
;; 判断值是否为 range 类型。
;;
;; 语法
;; ----
;; (range? x)
;;
;; 参数
;; ----
;; x : any
;; 要判断的值。
;;
;; 返回值
;; ----
;; boolean
;; 如果是 range，返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (range? (numeric-range 0 5)) => #t
;; (range? "hello") => #f
;; (range? '(1 2 3)) => #f
;;
;; 错误处理
;; ----
;; 无

(check-true (range? (numeric-range 0 5)))
(check-true (range? (range 5 (lambda (i) i))))
(check-false (range? "hello"))
(check-false (range? '(1 2 3)))
(check-false (range? #(1 2 3)))
(check-false (range? 42))

(check-report)
