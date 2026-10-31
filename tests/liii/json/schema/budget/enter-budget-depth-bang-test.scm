(import (liii check) (liii json schema budget))

(check-set-mode! 'report-failed)

;; enter-budget-depth!
;; 进入一层求值深度并返回新的深度。
;;
;; 语法
;; ----
;; (enter-budget-depth! budget)
;;
;; 参数
;; ----
;; budget : budget
;; 由 make-resource-budget 创建的预算记录。
;;
;; 返回值
;; ----
;; integer
;; 进入后的求值深度。
;;
;; 注意
;; ----
;; 深度超过 max-depth 上限时立即抛出 json-schema-budget-exceeded，
;; 不会等到本次校验结束。
;;
;; 示例
;; ----
;; (enter-budget-depth! budget) => 1
;;
;; 错误处理
;; ----
;; json-schema-budget-exceeded 当深度超过上限时。

(define budget (make-resource-budget 10 100 1000))

(check (enter-budget-depth! budget) => 1)
(check (budget-depth budget) => 1)
(check (enter-budget-depth! budget) => 2)
(check (budget-depth budget) => 2)

;; 超过上限时抛出预算异常

(define tiny (make-resource-budget 1 100 100))
(enter-budget-depth! tiny)
(check-catch 'json-schema-budget-exceeded (enter-budget-depth! tiny))

(check-report)
