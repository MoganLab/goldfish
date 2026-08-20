;; (scheme repl) 模块文档与测试
;;
;; `(scheme repl)` 导出 interaction-environment 过程。
;;
;; ==== 过程 ====
;;
;;   (interaction-environment)   返回 REPL 所使用的交互环境
;;
;; ==== 说明 ====
;;
;; 1. 交互环境是程序顶层求值所使用的环境
;; 2. 常用于 (eval expr (interaction-environment)) 这类需求
;;
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc scheme/repl "interaction-environment"
(import (liii check) (scheme repl))
(check-set-mode! 'report-failed)

;; ==== 测试：interaction-environment ====
(check (procedure? interaction-environment) => #t)

(let ((env (interaction-environment)))
  (check (not (null? env)) => #t)
) ;let

;; ==== 测试：可用于 eval ====
(import (scheme eval))
(let ((env (interaction-environment)))
  (check (eval '(+ 40 2) env) => 42)
) ;let

(check-report)
