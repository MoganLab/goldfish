;; computation-environment-update!
;; 就地修改计算环境中的变量绑定。
;;
;; 语法
;; ----
;; (computation-environment-update! env var value)
;;
;; 参数
;; ----
;; env : computation-environment
;; 计算环境。
;; var : computation-environment-variable
;; 待绑定的环境变量。
;; value : any
;; 绑定值。
;; 返回值
;; ----
;; unspecified
;; 就地修改环境，返回值无意义。
;; 说明
;; ----
;; 与 computation-environment-update 的区别是：
;; 本过程直接修改传入的环境，不产生新环境。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(define v (make-computation-environment-variable 'x 100 #f))
(define env (make-computation-environment))

;; 就地修改后可以从同一环境读到新值
(computation-environment-update! env v 42)
(check (computation-environment-ref env v) => 42)

;; 再次修改覆盖旧值
(computation-environment-update! env v 7)
(check (computation-environment-ref env v) => 7)

(check-report)
