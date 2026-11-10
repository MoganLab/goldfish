;; lookup-env-var
;; 从自定义环境或进程环境变量中查找变量值。
;;
;; 语法
;; ----
;; (lookup-env-var name custom-env)
;;
;; 参数
;; ----
;; name : string?
;; 环境变量名。
;;
;; custom-env : list?
;; 自定义环境列表（包含 key-value 关联表），或空列表使用当前进程环境。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 环境变量值，未找到时返回 #f。

(import (liii check) (liii pi ai template))

(check-set-mode! 'report-failed)

(let ((env '(("MY_KEY" . "my_val"))))
  (check (lookup-env-var "MY_KEY" (list env)) => "my_val")
  (check (lookup-env-var "NOT_EXIST" (list env)) => #f)
) ;let

(check-report)
