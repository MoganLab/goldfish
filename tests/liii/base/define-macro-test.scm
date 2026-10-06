(import (liii check))
(import (liii base))

(check-set-mode! 'report-failed)

;; define-macro
;; 定义 Scheme 宏（展开器运行于宏定义时的词法环境中）。
;;
;; 语法
;; ----
;; (define-macro (name param ...) body ...)
;; (define-macro (name . rest) body ...)
;;
;; 参数
;; ----
;; name : symbol
;; 宏的名称。
;;
;; param ... : symbol
;; 宏形参，宏调用时实参表达式不会被求值，而是作为 S 表达式直接传入。
;;
;; rest : symbol
;; 剩余实参列表。
;;
;; body ... : any
;; 宏体表达式，展开时求值并返回新的代码（AST），该代码随后在调用点求值。
;;
;; 返回值
;; -----
;; macro
;; 返回定义的宏。
;;
;; 说明
;; ----
;; define-macro 是 Lisp 风格的宏（类似 defmacro）。
;; 与过程（procedure）不同，宏的参数在传入时不进行求值。
;; 宏体在求值展开时，处于宏【定义时】的词法环境中（而非调用点环境）。
;; 使用 (macro? obj) 可以判断对象是否为宏。

;; 用例 1：基本代码替换宏（交换两个变量）
(define-macro (my-swap! a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a)) (set! ,a ,b) (set! ,b ,tmp))
  ) ;let
) ;define-macro

(let ((x 1) (y 2))
  (my-swap! x y)
  (check x => 2)
  (check y => 1)
) ;let

;; 用例 2：宏参数不预先求值（控制流宏）
(define-macro (my-when test . body) `(if ,test (begin ,@body) ,#f))

(let ((flag #f))
  (my-when #f (set! flag #t))
  (check flag => #f)
  (my-when #t (set! flag #t))
  (check flag => #t)
) ;let

;; 用例 3：宏展开器运行于定义时的词法环境

(define macro-scope-var "defined-scope")
(define-macro (get-macro-scope) macro-scope-var)

(check (let ((macro-scope-var "caller-scope"))
         (get-macro-scope)
       ) ;let
  =>
  "defined-scope"
) ;check

;; 用例 4：macro? 谓词判断
(check (macro? my-swap!) => #t)
(check (macro? (lambda (x) x)) => #f)

(check-report)
