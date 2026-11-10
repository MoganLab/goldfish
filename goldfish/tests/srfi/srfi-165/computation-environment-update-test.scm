;; computation-environment-update
;; 在计算环境的副本上绑定变量，返回新环境。
;;
;; 语法
;; ----
;; (computation-environment-update env var value ...)
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
;; computation-environment
;; 返回绑定后的新环境；原环境保持不变。
;; 说明
;; ----
;; 可以一次绑定多个变量，绑定按从左到右的顺序生效。
;; 预定义变量就地写入新环境的槽位，运行期变量进入局部绑定表。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(define v (make-computation-environment-variable 'x 100 #f))
(define w (make-computation-environment-variable 'y 200 #f))
(define env (make-computation-environment))

;; 绑定一个变量
(define env1 (computation-environment-update env v 42))
(check (computation-environment-ref env1 v) => 42)

;; 原环境保持不变
(check (computation-environment-ref env v) => 100)

;; 一次绑定多个变量
(define env2 (computation-environment-update env v 1 w 2))
(check (computation-environment-ref env2 v) => 1)
(check (computation-environment-ref env2 w) => 2)

(check-report)
