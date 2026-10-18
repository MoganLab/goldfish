(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-load-models-store
;; 从 models-store.json 读取模型库信息缓存。
(let ((models-store (pi-load-models-store "tools/pi/tests/fixtures/default")))
  (check-true (pair? models-store)))

(check-report)
