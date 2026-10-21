(import (liii check)
  (liii pi read)
) ;import

(check-set-mode! 'report-failed)

;; pi-proc-accepts-args?：按 arity 判断过程能否接受 n 个参数
;; 用于在调用带 opts 的工具之前判定是否需要退回无 opts 调用，
;; 从而避免用 catch 兜底后重跑（那会把工具自身的失败也重跑一遍）

;; 1. 定长过程
(check-true (pi-proc-accepts-args? (lambda (a) a) 1))
(check-false (pi-proc-accepts-args? (lambda (a) a) 2))

;; 2. 两个定长参数
(check-true (pi-proc-accepts-args? (lambda (a b) a) 2))
(check-false (pi-proc-accepts-args? (lambda (a b) a) 3))

;; 3. 可变参数过程
(check-true (pi-proc-accepts-args? (lambda (a . rest) a) 5))
(check-true (pi-proc-accepts-args? (lambda args args) 5))

(check-report)
