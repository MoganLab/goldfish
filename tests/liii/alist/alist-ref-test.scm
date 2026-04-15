(import (liii alist) (liii check))


(check-set-mode! 'report-failed)


;; alist-ref
;; 在关联列表中按键查找对应的值。
;;
;; 语法
;; ----
;; (alist-ref alist key)
;; (alist-ref alist key thunk)
;; (alist-ref alist key thunk =)
;;
;; 参数
;; ----
;; alist : list
;; 关联列表，每个元素应为pair。
;;
;; key : any
;; 要查找的键。
;;
;; thunk : procedure 可选
;; 当键未找到时调用的零参数过程。
;;
;; = : procedure 可选
;; 自定义比较过程，默认使用eqv?。
;;
;; 返回值
;; ----
;; any
;; 返回与key匹配的值；如果元素是点对，则返回cdr的内容。
;;
;; 注意
;; ----
;; 未提供thunk时，键不存在会抛出`key-error`。
;;
;; 示例
;; ----
;; (alist-ref '((a 1)) 'a) => '(1)
;; (alist-ref '((a . 1)) 'a) => 1
;; (alist-ref '((a . 1)) 'b (lambda () 2)) => 2
;;
;; 错误处理
;; ----
;; key-error 当键不存在且未提供thunk时抛出。


(check (alist-ref '((a 1)) 'a) => '(1))
(check (alist-ref '((a . 1)) 'a) => 1)
(check-catch 'key-error
  (alist-ref '(("a" . 1)) "a")
) ;check-catch
(check-catch 'key-error
  (alist-ref '((a . 1)) 'b)
) ;check-catch
(check (alist-ref '((a . 1)) 'b (lambda () 2))
  =>
  2
) ;check


(check-report)
