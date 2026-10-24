(import (liii check) (liii pi code read) (liii pi code session))

(check-set-mode! 'report-failed)

;; pi-read-schema
;; 获取 read 工具的 JSON Schema 定义。
(let ((schema (pi-read-schema)))
  (check (cdr (assoc "type" schema)) => "object")
  (check-true (pair? (assoc "properties" schema))))

(check-report)
