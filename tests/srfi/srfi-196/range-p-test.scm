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
;; 错误处理
;; ----
;; 无（谓词对任何值都返回布尔值，不抛错）

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(check-true (range? (numeric-range 0 5)))
(check-true
  (range? (range 5 (lambda (i) i)))
) ;check-true
(check-false (range? "hello"))
(check-false (range? '(1 2 3)))
(check-false (range? #(1 2 3)))
(check-false (range? 42))
(check-false (range? 'sym))
(check-false (range? #f))
(check-false (range? (lambda (i) i)))


(check-report)
