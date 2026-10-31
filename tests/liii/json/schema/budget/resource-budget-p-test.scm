(import (liii check) (liii json schema budget))

(check-set-mode! 'report-failed)

;; resource-budget?
;; 判断对象是否为资源预算。
;;
;; 语法
;; ----
;; (resource-budget? value)
;;
;; 参数
;; ----
;; value : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 是资源预算返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 预算记录是长度为 6 的向量。
;;
;; 示例
;; ----
;; (resource-budget? (make-resource-budget 10 100 1000)) => #t
;;
;; 错误处理
;; ----
;; 无。

(check-true (resource-budget? (make-resource-budget 10 100 1000)))
(check-false (resource-budget? 'not-a-budget))
(check-false (resource-budget? #t))
(check-false (resource-budget? '(1 2 3)))
(check-false (resource-budget? (vector 0 0 0)))

(check-report)
