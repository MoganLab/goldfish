(import (liii check))
(import (scheme let))


(check-set-mode! 'report-failed)


;; curlet
;; 返回当前环境的 let（即当前词法环境）。
;;
;; 语法
;; ----
;; (curlet)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ------
;; let?
;; 当前词法环境对应的 let。
;;
;; 说明
;; ----
;; curlet 返回当前求值环境的 let 对象。
;; 在不同位置调用 curlet 会得到不同的 let。
;; rootlet 是所有环境的根环境，curlet 在顶层调用时返回的环境
;; 其 outlet 链最终指向 rootlet。


;; curlet 返回一个 let
(check (let? (curlet)) => #t)


;; 在 let 内部调用 curlet，得到包含该 let 绑定的环境
(check (let-ref (let ((a 42)) (curlet)) 'a) => 42)


;; 在 lambda 内部，curlet 反映函数的环境
(check (let-ref ((lambda () (let ((x 99)) (curlet)))) 'x) => 99)


;; 多次调用 curlet 在同一位置返回的 let 内容一致（都包含 a）
(check (let-ref (let ((a 7)) (curlet)) 'a) => 7)


;; curlet 的 outlet 链可达 rootlet
(check (let? (outlet (curlet))) => #t)


(check-report)
