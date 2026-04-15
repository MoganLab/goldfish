(import (liii check)
  (scheme base)
  (scheme eval)
) ;import
(check-set-mode! 'report-failed)
;; eval
;; 在指定环境中对 Scheme 表达式求值。
;;
;; 语法
;; ----
;; (eval expression environment)
;;
;; 参数
;; ----
;; expression : any
;; 要求值的 Scheme 表达式。
;;
;; environment : environment
;; 由 `environment` 构造的求值环境。
;;
;; 返回值
;; ----
;; any
;; `expression` 在指定环境中求值后的结果。
;;
;; 描述
;; ----
;; `eval` 在给定环境中执行表达式，并返回结果。对同一个环境多次调用 `eval`
;; 时，先前定义的绑定会保留下来，因此可用于增量求值。
;;
;; 示例
;; ----
;; (eval '(+ 1 2) (environment '(scheme base))) => 3
;;
;; 错误处理
;; --------
;; unbound-variable
;; 当表达式引用环境中不存在的绑定时抛出错误。
(check (eval '(+ 1 2)
         (environment '(scheme base))
       ) ;eval
  =>
  3
) ;check
(let ((env (environment '(scheme base))))
  (check (eval '(begin (define answer 41) (+ answer 1))
           env
         ) ;eval
    =>
    42
  ) ;check
  (check (eval 'answer env) => 41)
  (check (eval '(set! answer (+ answer 1)) env)
    =>
    42
  ) ;check
  (check (eval 'answer env) => 42)
) ;let
(check-catch 'unbound-variable
  (eval 'x
    (environment '(only (scheme base) square)
    ) ;environment
  ) ;eval
) ;check-catch
(check-report)
