(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-bash-schema
;; 获取 bash 工具的 JSON Schema 定义。
(let ((schema (pi-bash-schema)))
  (check (cdr (assoc "type" schema)) => "object")
  (check-true (pair? (assoc "properties" schema))))

(check-report)
