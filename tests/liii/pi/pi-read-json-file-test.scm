(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-read-json-file
;; 读取并解析 JSON 配置文件。
(check (pi-read-json-file "non_existent_file.json") => #f)
(let ((data (pi-read-json-file "tools/pi/tests/fixtures/default/settings.json")))
  (check-true (pair? data)))

(check-report)
