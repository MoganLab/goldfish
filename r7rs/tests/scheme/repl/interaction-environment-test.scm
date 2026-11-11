;; interaction-environment
;; 返回交互式求值环境说明符（REPL 顶层环境）。
;;
;; 语法
;; ----
;; (interaction-environment)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; environment
;; 代表交互求值环境的对象，可作为求值环境传递给 `eval` 或 `load`。
;;
;; 描述
;; ----
;; 1. `interaction-environment` 返回当前会话交互环境说明符（在 Goldfish 中对应全局顶层 rootlet）。
;; 2. 传递给 `eval` 时，表达式将在顶层环境中求值，所定义的绑定在后续求值和当前会话中可见。
;; 3. 与 `(environment ...)` 返回的独立隔离环境不同，`interaction-environment` 反映交互式系统的全局环境。
;;
;; (eval '(+ 1 2) (interaction-environment)) => 3
;;
;; 错误处理
;; --------
;; 无。

(import (liii check) (scheme base) (scheme eval) (scheme repl))
(check-set-mode! 'report-failed)

;; 1. interaction-environment 是过程
(check-true (procedure? interaction-environment))

;; 2. 返回环境说明符（在 s7 底层是 let 对象）
(define env (interaction-environment))
(check-true (let? env))

;; 3. 在交互环境中对表达式求值
(check (eval '(+ 1 2) env) => 3)

;; 4. 在交互环境中定义的绑定对后续可见
(eval '(define repl-test-var-1263 100) env)
(check (eval 'repl-test-var-1263 env) => 100)
(check repl-test-var-1263 => 100)

;; 5. 交互环境与 rootlet 保持一致
(check-true (eq? env (rootlet)))

(check-report)
