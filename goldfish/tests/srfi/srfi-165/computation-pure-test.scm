;; computation-pure
;; 创建返回常量值的计算。
;;
;; 语法
;; ----
;; (computation-pure value ...)
;;
;; 参数
;; ----
;; value ... : any
;; 计算返回的值，可有多个。
;; 返回值
;; ----
;; computation
;; 返回在求值时产出这些值的计算对象。
;; 说明
;; ----
;; 多值形式等价于 (values value ...)。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


;; 单值
(check (computation-run (computation-pure 42)) => 42)
(check (computation-run (computation-pure 'hello)) => 'hello)

;; 多值
(check (call-with-values (lambda () (computation-run (computation-pure 1 2 3))) list)
  =>
  '(1 2 3)
) ;check

(check-report)
