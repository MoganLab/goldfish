(import (liii check))
(import (scheme let))


(check-set-mode! 'report-failed)


;; rootlet
;; 返回全局根环境（rootlet）。所有内建函数和全局绑定都位于此处。
;;
;; 语法
;; ----
;; (rootlet)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ------
;; let?
;; 全局根环境。
;;
;; 说明
;; ----
;; rootlet 是所有环境的根。多次调用 rootlet 返回同一对象。
;; 所有内建函数（如 +、car）都能在 rootlet 中通过 symbol->value 找到。


;; rootlet 是一个 let
(check (let? (rootlet)) => #t)


;; 多次调用 rootlet 返回同一对象
(check (eq? (rootlet) (rootlet)) => #t)


;; rootlet 中包含内建符号 +
(check (symbol->value '+ (rootlet)) => +)


;; rootlet 中包含内建符号 car
(check (symbol->value 'car (rootlet)) => car)


;; rootlet 的 outlet 是 rootlet 自身
(check (eq? (outlet (rootlet)) (rootlet)) => #t)


(check-report)
