(import (liii check))
(import (liii base))

(check-set-mode! 'report-failed)

;; define-bacro
;; 定义绑定宏（Bacro，展开器运行于宏调用点的环境中）。
;;
;; 语法
;; ----
;; (define-bacro (name param ...) body ...)
;; (define-bacro (name . rest) body ...)
;;
;; 参数
;; ----
;; name : symbol
;; 宏的名称。
;;
;; param ... : symbol
;; 宏形参，宏调用时实参表达式不会被求值，作为 S 表达式直接传入。
;;
;; rest : symbol
;; 剩余实参列表。
;;
;; body ... : any
;; 宏体表达式，展开时在【调用点环境】中求值。
;;
;; 返回值
;; -----
;; bacro
;; 返回定义的绑定宏。
;;
;; 说明
;; ----
;; define-bacro 是 S7 Scheme 特有的宏机制。
;; 与 define-macro 的核心区别在于：
;; 1. define-macro 展开时运行在宏定义时的词法环境中；
;; 2. define-bacro 展开时运行在宏调用点（call-site）的环境中。
;;
;; 因此在 define-bacro 宏体中：
;; - (curlet) 返回的是调用者所在的环境，可用于捕获调用点上下文；
;; - 宏体内的自由变量在展开时从调用点的环境中查找解析。
;;
;; 使用 (macro? obj) 对 bacro 同样返回 #t。

;; 用例 1：自由变量从调用点环境中查找（与 define-macro 形成对比）

(define bacro-scope-var "defined-scope")
(define-bacro (get-bacro-scope) bacro-scope-var)

(check (let ((bacro-scope-var "caller-scope"))
         (get-bacro-scope)
       ) ;let
  =>
  "caller-scope"
) ;check

;; 用例 2：通过 (curlet) 捕获并检查调用者的环境变量
(define-bacro (get-caller-var var-sym) ((curlet) var-sym))

(check (let ((secret 42)) (get-caller-var secret)) => 42)

;; 用例 3：在调用点作用域内生成并执行表达式
(define-bacro (add-to-caller-x val) `(+ x ,val))

(check (let ((x 100)) (add-to-caller-x 23)) => 123)

;; 用例 4：macro? 谓词判断
(check (macro? get-bacro-scope) => #t)

(check-report)
