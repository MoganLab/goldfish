(import (liii alist) (liii check))


(check-set-mode! 'report-failed)


;; alist?
;; 判断对象是否为关联列表。
;;
;; 语法
;; ----
;; (alist? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 当obj是由pair组成的列表时返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 空列表`'()`也被视为合法的关联列表。
;;
;; 示例
;; ----
;; (alist? '()) => #t
;; (alist? '((a . 1))) => #t
;; (alist? '(1 2 3)) => #f
;;
;; 错误处理
;; ----
;; 无


(check (alist? '()) => #t)
(check (alist? '((a 1))) => #t)
(check (alist? '((a . 1))) => #t)
(check (alist? '((a . 1) (b . 2)))
  =>
  #t
) ;check
(check (alist? '(1 2 3)) => #f)


(check-report)
