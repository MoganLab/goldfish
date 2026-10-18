(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-read-json-file
;; 读取并解析 JSON 配置文件。
(define test-file
  (if (file-exists? "tests/fixtures/default/settings.json")
    "tests/fixtures/default/settings.json"
    "tools/pi/tests/fixtures/default/settings.json"
  ) ;if
) ;define

(check (pi-read-json-file "non_existent_file.json") => #f)
(let ((data (pi-read-json-file test-file)))
  (check-true (pair? data)))

(check-report)
