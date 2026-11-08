;; define-computation-type
;; 定义计算类型：创建环境的构造过程与运行计算的过程。
;;
;; 语法
;; ----
;; (define-computation-type make-environment run variable ...)
;;
;; 参数
;; ----
;; make-environment : symbol
;; 环境构造过程的名字。
;; run : symbol
;; 运行计算的过程的名字。
;; variable : list
;; 计算类型的静态字段，形如 (name default) 或 (name default immutable)；
;; 省略默认值时默认为 #f。
;; 返回值
;; ----
;; unspecified
;; 定义环境构造过程、运行过程以及各静态字段变量。
;; 说明
;; ----
;; 静态字段在环境中拥有固定槽位，
;; 可以通过 computation-environment-ref 读取、就地更新。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(define-computation-type make-show-env show-run
  (port (current-output-port))
  (col 0)
  (width 78)
) ;define-computation-type

;; 默认值
(check (show-run (computation-fn ((p port)) (computation-pure p))) => (current-output-port))
(check (show-run (computation-fn ((c col)) (computation-pure c))) => 0)
(check (show-run (computation-fn ((w width)) (computation-pure w))) => 78)

;; 静态字段可以被就地更新
(check
  (show-run
    (computation-bind
      (computation-with! (width 40))
      (lambda (_) (computation-fn ((w width)) (computation-pure w)))
    )
  )
  =>
  40
) ;check

(check-report)
