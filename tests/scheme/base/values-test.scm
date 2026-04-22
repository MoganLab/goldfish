(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; values
;; 返回多个值。
;;
;; 语法
;; ----
;; (values obj ...)
;;
;; 参数
;; ----
;; obj ... : 任意类型
;; 要返回的值。
;;
;; 返回值
;; ------
;; 多个值
;; 传入的所有值。
;;
;; 说明
;; ----
;; 1. 需要配合 call-with-values 使用
;; 2. 单值时行为与直接返回相同
(check (values 1) => 1)
(check (call-with-values (lambda () (values 1 2)) (lambda (a b) (+ a b))) => 3)
(check (call-with-values (lambda () (values)) (lambda args (length args))) => 1)
(check (call-with-values (lambda () (values 'a 'b 'c)) list) => '(a b c))

(check-report)
