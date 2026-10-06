(import (liii check) (liii syntax))

(check-set-mode! 'report-failed)

;; make-syntactic-closure
;; 创建一个句法闭包（syntactic closure）对象。
;;
;; 语法
;; ----
;; (make-syntactic-closure env free-vars expr)
;;
;; 参数
;; ----
;; env : environment
;; 闭包捕获的词法环境（如 (curlet)），expr 中的自由标识符将在该环境中解析。
;;
;; free-vars : list
;; 保持原样解析（不经 env 重命名）的自由变量列表，通常为 '()。
;;
;; expr : any
;; 被闭包包装的表达式，通常是标识符（符号）或 S 表达式。
;;
;; 返回值
;; ------
;; syntactic-closure
;; 新创建的句法闭包对象，其 rename 槽初始为 #f。
;;
;; 说明
;; ----
;; 句法闭包是 R7RS 卫生宏系统的底层机制：它将表达式与其定义时的词法环境
;; 绑定在一起，使宏展开后的标识符仍在定义点环境中解析，从而避免变量捕获。
;; (liii syntax) 中的实现基于 s7 的 C 扩展类型，由 src/s7_syntax_rules.c 提供。

;; 1. 包装符号
(let ((sc (make-syntactic-closure (curlet) '() 'x)))
  (check-true (syntactic-closure? sc))
  (check (syntactic-closure-expr sc) => 'x)
  (check (syntactic-closure-free-vars sc) => '())
  (check (syntactic-closure-rename sc) => #f)
) ;let

;; 2. 包装列表表达式
(let ((sc (make-syntactic-closure (rootlet) '() '(+ 1 2))))
  (check-true (syntactic-closure? sc))
  (check (syntactic-closure-expr sc) => '(+ 1 2))
) ;let

;; 3. env 槽保存创建时传入的环境
(let* ((e (curlet))
       (sc (make-syntactic-closure e '() 'x)))
  (check (eq? (syntactic-closure-env sc) e) => #t)
) ;let*

;; 4. free-vars 原样保存
(let ((sc (make-syntactic-closure (curlet) '(a b) 'x)))
  (check (syntactic-closure-free-vars sc) => '(a b))
) ;let

(check-report)
