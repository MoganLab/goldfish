;; get-environment-variables
;; 返回当前进程全部环境变量的键值对列表。
;;
;; 语法
;; ----
;; (get-environment-variables)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; alist
;; 形如 (("KEY" . "value") ...) 的字符串表，顺序未定义。

(import (liii check) (scheme process-context))

(check-set-mode! 'report-failed)

(let ((envs (get-environment-variables)))
  (check (list? envs) => #t)
  (let ((path-env (assoc "PATH" envs)))
    (check (pair? path-env) => #t)
    (check (string? (cdr path-env)) => #t)
  ) ;let
) ;let

(check-report)
