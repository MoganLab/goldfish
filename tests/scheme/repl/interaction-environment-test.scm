(import (liii check) (scheme repl) (scheme eval))
(check-set-mode! 'report-failed)

;; interaction-environment
;; 返回 REPL 所使用的交互环境。
;;
;; 语法
;; ----
;; (interaction-environment)
;;
;; 返回值
;; ----
;; environment

(check (procedure? interaction-environment) => #t)

(let ((env (interaction-environment)))
  (check (not (null? env)) => #t)
  (check (eval '(+ 40 2) env) => 42)
) ;let

(check-report)
