;; computation-fn
;; 创建读取环境变量并返回计算的宏。
;;
;; 语法
;; ----
;; (computation-fn (var ...) body ...)
;;
;; 参数
;; ----
;; var : symbol 或 (symbol symbol)
;; 要绑定的环境变量；
;; 写成 (name var) 时 name 为环境变量，var 为过程体内的绑定名。
;; body : expression ...
;; 过程体。
;; 返回值
;; ----
;; computation
;; 返回读取环境变量后执行函数体的计算对象。
;; 说明
;; ----
;; 过程体中可以像普通变量一样使用绑定的名字。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(define v (make-computation-environment-variable 'x 100 #f))

;; 直接绑定变量名
(check (computation-run (computation-fn (v) (computation-pure v))) => 100)

;; 使用别名绑定
(check (computation-run (computation-fn ((val v)) (computation-pure val))) => 100)

;; 绑定多个变量
(define w (make-computation-environment-variable 'y 200 #f))
(check
  (computation-run
    (computation-fn ((a v) (b w))
      (computation-pure (+ a b))
    )
  )
  =>
  300
) ;check

(check-report)
