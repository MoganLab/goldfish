(import (liii check))

;;; expand-eval : REPL 语义验证
;;; expand-eval 是 reader.scm 中 REPL/-e 的求值入口（展开 + 求值 core），
;;; 模拟 REPL 的交互场景：
;;; 1. 历史变量（C 层绑定到 rootlet，不经 expander 注册表）
;;; 2. 顶层 define 后跨形式引用
;;; 3. set! 已定义变量
;;; 4. 未定义变量 -> 求值期 unbound（可被 catch 捕获），而非展开期错误

;;; 历史变量 $1：由 C 层 s7_define 到 rootlet，expand-eval 通过 rootlet
;;; fallback 解析（eval 环境是 the-expander-library inlet）
(eval '(define $1 42) (rootlet))
(check (expand-eval '(+ $1 1)) => 43)

;;; 顶层 define 后跨形式引用
(expand-eval '(define y 7))
(check (expand-eval '(+ y 1)) => 8)

;;; set! 已定义变量
(expand-eval '(set! y 10))
(check (expand-eval 'y) => 10)

;;; 未定义变量：求值期报 unbound，不是展开期错误
(check (catch #t (lambda () (expand-eval 'undefined-var)) (lambda args #f)) => #f)

(check-report)
