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

(import (liii check) (scheme char) (scheme process-context))

(check-set-mode! 'report-failed)

(let ((envs (get-environment-variables)))
  (check (list? envs) => #t)
  ;; Windows 环境变量名大小写不敏感（runner 上常见为 "Path"），按名不区分大小写查找
  (let ((path-env
          (let loop ((rest envs))
            (cond
              ((null? rest) #f)
              ((string-ci=? (caar rest) "PATH") (car rest))
              (else (loop (cdr rest)))
            ) ;cond
          ) ;let
         )
        ) ;
    (check (pair? path-env) => #t)
    (check (string? (cdr path-env)) => #t)
  ) ;let
) ;let

(check-report)
