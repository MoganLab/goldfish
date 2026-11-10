;; computation-bind
;; 把计算结果传给后续过程继续计算。
;;
;; 语法
;; ----
;; (computation-bind computation proc ...)
;;
;; 参数
;; ----
;; computation : computation
;; 提供结果的计算对象。
;; proc : procedure
;; 接收前一计算的结果并返回新计算对象的过程。
;; 返回值
;; ----
;; computation
;; 返回组合后的计算对象。
;; 说明
;; ----
;; 可以链式绑定多个过程：
;; 前一个过程的结果会作为后一个过程的输入。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


;; 单次绑定
(check (computation-run (computation-bind (computation-pure 10)
                          (lambda (x) (computation-pure (* x 2)))
                        ) ;computation-bind
       ) ;computation-run
  =>
  20
) ;check

;; 多值绑定
(check (computation-run (computation-bind (computation-pure 1 2)
                          (lambda (a b) (computation-pure (+ a b)))
                        ) ;computation-bind
       ) ;computation-run
  =>
  3
) ;check

;; 链式绑定
(check
  (computation-run
    (computation-bind
      (computation-pure 10)
      (lambda (x)
        (computation-bind
          (computation-pure 20)
          (lambda (y) (computation-pure (+ x y)))
        ) ;computation-bind
      ) ;lambda
    ) ;computation-bind
  ) ;computation-run
  =>
  30
) ;check

(check-report)
