(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; call-with-values
;; 调用产生多值的过程，并将值传递给消费过程。
;;
;; 语法
;; ----
;; (call-with-values producer consumer)
;;
;; 参数
;; ----
;; producer : procedure?
;; 无参数过程，返回多个值。
;; consumer : procedure?
;; 接收 producer 返回值的过程。
;;
;; 返回值
;; ------
;; 任意类型
;; consumer 的返回值。
;;
;; 说明
;; ----
;; 1. producer 必须返回多个值
;; 2. consumer 的参数个数应与 producer 返回值个数匹配
;; 3. 是接收多值的标准方式
(check (call-with-values (lambda () (values 1 2)) +) => 3)
(check (call-with-values (lambda () (values 10 20 30)) (lambda (a b c) (* a b c)))
  =>
  6000
) ;check
(check (call-with-values (lambda () (values)) (lambda args (length args))) => 1)
(check (call-with-values (lambda () (values 'x)) list) => '(x))

(check-report)
