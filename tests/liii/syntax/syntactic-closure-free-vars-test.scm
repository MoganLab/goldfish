(import (liii check) (liii syntax))

(check-set-mode! 'report-failed)

;; syntactic-closure-free-vars
;; 取出句法闭包的自由变量列表。
;;
;; 语法
;; ----
;; (syntactic-closure-free-vars sc)
;;
;; 参数
;; ----
;; sc : syntactic-closure
;; 句法闭包对象。
;;
;; 返回值
;; ------
;; list
;; 创建闭包时传入的 free-vars 列表，原样返回。
;;
;; 说明
;; ----
;; free-vars 列出闭包中不做词法重命名、保持按调用点解析的标识符。
;; 通常传 '()，表示所有自由标识符都在闭包环境中解析。
;;
;; 错误处理
;; --------
;; 参数不是句法闭包时抛出 type-error。

(check (syntactic-closure-free-vars (make-syntactic-closure (curlet) '() 'x)) => '())
(check (syntactic-closure-free-vars (make-syntactic-closure (curlet) '(a) 'x)) => '(a))
(check (syntactic-closure-free-vars (make-syntactic-closure (curlet) '(a b c) 'x)) => '(a b c))

;; 错误：非句法闭包参数
(check-catch 'type-error (syntactic-closure-free-vars 'x))
(check-catch 'type-error (syntactic-closure-free-vars #(1 2)))

(check-report)
