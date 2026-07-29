(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; sublet
;; 在已有 let 的环境内创建一个新的子 let，并初始化若干绑定。
;;
;; 语法
;; ----
;; (sublet parent-let)
;; (sublet parent-let symbol value ...)
;;
;; 参数
;; ----
;; parent-let : let?
;; 父环境（新 let 的 outlet 指向它）。
;;
;; symbol : symbol?
;; 绑定名。
;;
;; value : any
;; 绑定值。
;;
;; 返回值
;; ------
;; let?
;; 新创建的子 let。
;;
;; 说明
;; ----
;; sublet 创建的新 let 本身只包含新加入的绑定，
;; 但其 outlet 指向 parent-let，因此可通过环境链访问父绑定。
;; 这与 varlet 不同：varlet 把绑定直接合并进 target-let。


;; sublet 返回一个 let
(check (let? (sublet (inlet 'a 1))) => #t)


;; sublet 新 let 包含新加入的绑定
(check (let-ref (sublet (inlet 'a 1) 'b 2) 'b) => 2)


;; sublet 新 let 的 outlet 指向父 let
(check (let ((parent (inlet 'a 1)))
         (eq? (outlet (sublet parent 'b 2)) parent)
       ) ;let
  =>
  #t
) ;check


;; sublet 新 let 本身不包含父绑定（let->list 只显示直接绑定）
(check (let->list (sublet (inlet 'a 1) 'b 2)) => (list (cons 'b 2)))


;; sublet 新 let 长度只反映直接绑定数
(check (length (sublet (inlet 'a 1) 'b 2)) => 1)


;; 空 sublet 也返回 let
(check (let? (sublet (inlet))) => #t)


(check-report)
