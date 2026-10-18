(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-ls-schema
;; 获取 ls 工具的 JSON Schema 定义。
(let ((schema (pi-ls-schema)))
  (check (cdr (assoc "type" schema)) => "object")
  (check-true (pair? (assoc "properties" schema))))

(check-report)
