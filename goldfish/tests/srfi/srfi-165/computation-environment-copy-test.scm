;; computation-environment-copy
;; 复制计算环境。
;;
;; 语法
;; ----
;; (computation-environment-copy env)
;;
;; 参数
;; ----
;; env : computation-environment
;; 待复制的计算环境。
;; 返回值
;; ----
;; computation-environment
;; 返回与 env 内容一致的新环境。
;; 说明
;; ----
;; 复制后的环境与原环境相互独立：
;; 在副本上更新预定义变量不会影响原环境。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(define v (make-computation-environment-variable 'x 100 #f))
(define env (make-computation-environment))

(computation-environment-update! env v 42)

(define copy (computation-environment-copy env))
(check (computation-environment-ref copy v) => 42)

;; 修改副本不影响原环境
(computation-environment-update! copy v 7)
(check (computation-environment-ref copy v) => 7)
(check (computation-environment-ref env v) => 42)

(check-report)
