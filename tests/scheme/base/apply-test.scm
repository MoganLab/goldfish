(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; apply
;; 将参数列表应用于过程。
;;
;; 语法
;; ----
;; (apply proc args)
;; (apply proc arg1 ... args)
;;
;; 参数
;; ----
;; proc : procedure?
;; 待调用的过程。
;; args : list?
;; 参数列表。
;;
;; 返回值
;; ------
;; 任意类型
;; proc 的返回值。
;;
;; 说明
;; ----
;; 1. 最后一个参数必须是列表
;; 2. 列表中的元素作为独立参数传递给过程
(check (apply + '(1 2 3)) => 6)
(check (apply + 1 2 '(3 4)) => 10)
(check (apply list '()) => '())
(check (apply string-append '("a" "b" "c")) => "abc")
(check-catch 'syntax-error
  (apply 1 '(2 3))
) ;check-catch

(check-report)
