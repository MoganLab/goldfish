(import (liii check))
(import (scheme let))


(check-set-mode! 'report-failed)


;; let->list
;; 将 let 的所有直接绑定转换为点对列表。
;;
;; 语法
;; ----
;; (let->list let)
;;
;; 参数
;; ----
;; let : let?
;; 要转换的 let。
;;
;; 返回值
;; ------
;; list?
;; 由 (symbol . value) 点对组成的列表，每个点对对应一个直接绑定。
;;
;; 说明
;; ----
;; let->list 返回 let 的所有直接绑定（不包含 outlet 链上的绑定）。
;; 每个绑定表示为 (symbol . value) 点对。
;; 返回列表的长度等于 let 的直接绑定数量。


;; 空 inlet 转换为空列表
(check (let->list (inlet)) => (list))


;; 单绑定的 inlet
(check (let->list (inlet 'a 1)) => (list (cons 'a 1)))


;; 多绑定的 inlet
(check (let->list (inlet 'a 1 'b 2)) => (list (cons 'a 1) (cons 'b 2)))


;; let->list 的长度等于绑定数
(check (length (let->list (inlet 'a 1 'b 2 'c 3))) => 3)


;; let->list 只包含直接绑定，不含 outlet 链上的
(check (let->list (sublet (inlet 'a 1) 'b 2)) => (list (cons 'b 2)))


;; let-set! 后 let->list 反映新值
(check (let ((e (inlet 'a 1)))
         (let-set! e 'a 99)
         (let->list e)
       ) ;let
  =>
  (list (cons 'a 99))
) ;check


(check-report)
