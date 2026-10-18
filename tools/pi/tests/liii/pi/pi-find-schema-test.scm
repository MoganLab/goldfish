(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-find-schema
;; 获取 find 工具的 JSON Schema 定义。
(let ((schema (pi-find-schema)))
  (check (cdr (assoc "type" schema)) => "object")
  (check-true (pair? (assoc "properties" schema))))

(check-report)
