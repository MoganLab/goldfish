(import (liii check) (liii syntax))

(check-set-mode! 'report-failed)

;; syntactic-closure-expr
;; 取出句法闭包包装的表达式。
;;
;; 语法
;; ----
;; (syntactic-closure-expr sc)
;;
;; 参数
;; ----
;; sc : syntactic-closure
;; 句法闭包对象。
;;
;; 返回值
;; ------
;; any
;; 创建闭包时传入的 expr，原样返回（不做任何展开或剥离）。
;;
;; 错误处理
;; --------
;; 参数不是句法闭包时抛出 type-error。

;; 1. 符号表达式
(check (syntactic-closure-expr (make-syntactic-closure (curlet) '() 'x)) => 'x)

;; 2. 列表表达式
(check (syntactic-closure-expr (make-syntactic-closure (curlet) '() '(+ 1 2))) => '(+ 1 2))

;; 3. 常量表达式
(check (syntactic-closure-expr (make-syntactic-closure (curlet) '() 42)) => 42)
(check (syntactic-closure-expr (make-syntactic-closure (curlet) '() "s")) => "s")

;; 4. 嵌套句法闭包：expr 也可以是另一个句法闭包
(let* ((inner (make-syntactic-closure (curlet) '() 'x))
       (outer (make-syntactic-closure (curlet) '() inner)))
  (check (eq? (syntactic-closure-expr outer) inner) => #t)
) ;let*

;; 错误：非句法闭包参数
(check-catch 'type-error (syntactic-closure-expr 'x))
(check-catch 'type-error (syntactic-closure-expr 42))

(check-report)
