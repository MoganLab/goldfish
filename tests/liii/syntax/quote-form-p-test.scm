(import (liii check) (liii syntax))

(check-set-mode! 'report-failed)

;; quote-form?
;; 判断一个对象是否为 quote 形式。
;;
;; 语法
;; ----
;; (quote-form? x)
;;
;; 参数
;; ----
;; x : any
;; 待判定的对象。
;;
;; 返回值
;; ------
;; boolean
;; 当 x 是恰好带一个参数的 quote 形式时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; quote-form? 接受两种等价的 quote 形式：
;; - (quote x)：car 为普通符号 quote；
;; - (#_quote x)：s7 reader 将 'x 读为 (#_quote x)，#_quote 是驻留的语法对象；
;; - 形式必须恰好带一个参数：cdr 为 pair 且 cddr 为空列表。
;; 该函数常用于遍历代码或数据时穿透 quote 包装，如 (liii tree) 的 tree-depth。

;; 1. 符号 quote 形式（car 为普通符号 quote）
(check (quote-form? (list 'quote 'a)) => #t)
(check (quote-form? (list 'quote '(1 2 3))) => #t)
(check (quote-form? (cons 'quote '(a))) => #t)

;; 2. #_quote 语法对象形式（s7 reader 将 'x 读为 (#_quote x)）
(check (quote-form? ''a) => #t)
(check (quote-form? ''(1 2 3)) => #t)
(check (quote-form? '(#_quote a)) => #t)
(check (quote-form? '(#_quote (a b))) => #t)

;; 3. 非法结构：参数个数不符或带点对尾部
(check (quote-form? '(quote)) => #f)
(check (quote-form? '(quote a b)) => #f)
(check (quote-form? '(quote . a)) => #f)

;; 4. 非 quote 对象
(check (quote-form? 'quote) => #f)
(check (quote-form? '()) => #f)
(check (quote-form? 5) => #f)
(check (quote-form? "quote") => #f)
(check (quote-form? '(quasiquote a)) => #f)
(check (quote-form? '(unquote a)) => #f)
(check (quote-form? '(unquote-splicing a)) => #f)
(check (quote-form? '(if x y)) => #f)

(check-report)
