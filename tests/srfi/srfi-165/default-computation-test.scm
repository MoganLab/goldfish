;; default-computation
;; 默认计算变量。
;;
;; 语法
;; ----
;; default-computation
;;
;; 参数
;; ----
;; 无
;; 返回值
;; ----
;; computation-environment-variable
;; 默认计算环境变量。
;; 说明
;; ----
;; 当一个计算对象不是过程时，
;; 运行环境会把它当作对 default-computation 的引用求值。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(define env (make-computation-environment))

;; 默认值为 #f
(check (computation-environment-ref env default-computation) => #f)

;; 绑定后可以被读取
(define env1 (computation-environment-update env default-computation 'custom))
(check (computation-environment-ref env1 default-computation) => 'custom)

(check-report)
