(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-edit-schema
;; 获取 edit 工具的 JSON Schema 定义。
(let ((schema (pi-edit-schema)))
  (check (cdr (assoc "type" schema)) => "object")
  (check-true (pair? (assoc "properties" schema))))

(check-report)
