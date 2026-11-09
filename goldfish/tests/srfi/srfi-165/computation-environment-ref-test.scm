;; computation-environment-ref
;; 读取计算环境中变量的值。
;;
;; 语法
;; ----
;; (computation-environment-ref env var)
;;
;; 参数
;; ----
;; env : computation-environment
;; 计算环境。
;; var : computation-environment-variable
;; 待读取的环境变量。
;; 返回值
;; ----
;; any
;; 返回变量在环境中的当前值；变量未被绑定时返回其默认值。
;; 说明
;; ----
;; 预定义变量（计算类型中的静态字段）存放在环境的向量槽位中，
;; 运行期变量存放在局部绑定表中，两者都通过本过程读取。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(define v (make-computation-environment-variable 'x 100 #f))
(define env (make-computation-environment))

;; 未绑定时返回默认值
(check (computation-environment-ref env v) => 100)

;; 绑定后返回绑定的值
(check (computation-environment-ref (computation-environment-update env v 42) v) => 42)

;; 原环境不受影响
(check (computation-environment-ref env v) => 100)

(check-report)
