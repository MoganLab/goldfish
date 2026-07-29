(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; outlet
;; 返回给定 let 的外层（父）环境。
;;
;; 语法
;; ----
;; (outlet let)
;;
;; 参数
;; ----
;; let : let?
;; 要查询外层环境的 let。
;;
;; 返回值
;; ------
;; let?
;; 该 let 的外层环境。rootlet 的 outlet 是 rootlet 自身。
;;
;; 说明
;; ----
;; outlet 用于遍历环境的父链。
;; 通过 sublet 等构造的环境，其 outlet 指向父环境。
;; rootlet 是环境的根，其 outlet 指向自身。


;; rootlet 的 outlet 是 rootlet 自身
(check (eq? (outlet (rootlet)) (rootlet)) => #t)


;; curlet 的 outlet 是一个 let
(check (let? (outlet (curlet))) => #t)


;; inlet 创建的独立环境，其 outlet 是 rootlet
(check (eq? (outlet (inlet 'a 1)) (rootlet)) => #t)


;; sublet 创建的环境，其 outlet 指向父 let
(check (let ((parent (inlet 'a 1)))
         (eq? (outlet (sublet parent 'b 2)) parent)
       ) ;let
  =>
  #t
) ;check


;; outlet 链最终到达 rootlet
(check (eq? (outlet (outlet (sublet (inlet 'a 1) 'b 2))) (rootlet)) => #t)


(check-report)
