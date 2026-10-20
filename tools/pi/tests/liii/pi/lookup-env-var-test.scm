(import (liii check) (liii pi template))

(check-set-mode! 'report-failed)

;; lookup-env-var
;; 从可选环境列表中查找指定环境变量的值。
(let ((env '(("MY_KEY" . "my_val"))))
  (check (lookup-env-var "MY_KEY" (list env)) => "my_val")
  (check (lookup-env-var "NOT_EXIST" (list env)) => #f))

(check-report)
