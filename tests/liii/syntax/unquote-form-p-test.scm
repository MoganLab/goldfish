(import (liii check) (liii syntax))

(check-set-mode! 'report-failed)

;; unquote-form?
;; 判断一个对象是否为 unquote 形式。
;;
;; 语法
;; ----
;; (unquote-form? x)
;;
;; 参数
;; ----
;; x : any
;; 待判定的对象。
;;
;; 返回值
;; ------
;; boolean
;; 当 x 是恰好带一个参数的 (unquote ...) 或 (unquote-splicing ...) 形式时返回 #t，
;; 否则返回 #f。
;;
;; 说明
;; ----
;; unquote-form? 接受两种 unquote 形式：
;; - (unquote x)：s7 reader 将 ,x 读为 (unquote x)，unquote 是普通符号；
;; - (unquote-splicing x)：规范化后的 splicing 形式；
;; - 形式必须恰好带一个参数：cdr 为 pair 且 cddr 为空列表。
;; 该函数常用于遍历准引用（quasiquote）结构时识别插值点。

;; 1. (unquote x) 形式（car 为普通符号 unquote）
(check (unquote-form? (list 'unquote 'x)) => #t)
(check (unquote-form? (list 'unquote '(+ 1 2))) => #t)
(check (unquote-form? (cons 'unquote '(x))) => #t)

;; 2. (unquote-splicing x) 形式
(check (unquote-form? (list 'unquote-splicing 'x)) => #t)
(check (unquote-form? (list 'unquote-splicing '(a b))) => #t)

;; 3. 非法结构：参数个数不符或带点对尾部
(check (unquote-form? '(unquote)) => #f)
(check (unquote-form? '(unquote x y)) => #f)
(check (unquote-form? '(unquote . x)) => #f)
(check (unquote-form? '(unquote-splicing)) => #f)
(check (unquote-form? '(unquote-splicing x y)) => #f)

;; 4. 非 unquote 对象
(check (unquote-form? 'unquote) => #f)
(check (unquote-form? '()) => #f)
(check (unquote-form? 5) => #f)
(check (unquote-form? "unquote") => #f)
(check (unquote-form? '(quote x)) => #f)
(check (unquote-form? '(quasiquote x)) => #f)
(check (unquote-form? '(if x y)) => #f)

(check-report)
