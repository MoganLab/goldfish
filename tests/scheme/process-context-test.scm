(import (srfi srfi-78) (scheme process-context) (srfi srfi-13) (liii os))
(check-set-mode! 'report-failed)
(when (os-linux?)
  (check (string-prefix? "/" (get-environment-variable "HOME")) => #t)
) ;when
(when (os-linux?)
  (let ((envs (get-environment-variables)))
    (check (list? envs) => #t)
    (let ((home-env (assoc "HOME" envs)))
      (check (pair? home-env) => #t)
      (check (string-prefix? "/" (cdr home-env)) => #t)
    ) ;let
    (check (pair? (assoc "PATH" envs)) => #t)
  ) ;let
) ;when

;; command-line：返回命令行参数列表
(check (list? (command-line)) => #t)

;; exit / emergency-exit：过程存在
(check (procedure? exit) => #t)
(check (procedure? emergency-exit) => #t)

(check-report)
