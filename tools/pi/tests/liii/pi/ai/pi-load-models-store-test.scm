(import (liii check) (liii pi ai models))

(check-set-mode! 'report-failed)

;; pi-load-models-store
;; 从 models-store.json 读取模型库信息缓存。
(define fixture-dir
  (if (file-exists? "tests/fixtures/default")
    "tests/fixtures/default"
    "tools/pi/tests/fixtures/default"
  ) ;if
) ;define

(let ((models-store (pi-load-models-store fixture-dir)))
  (check-true (pair? models-store)))

(check-report)
