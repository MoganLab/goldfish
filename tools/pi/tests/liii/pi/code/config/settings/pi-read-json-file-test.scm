;; pi-read-json-file
;; 读取并解析 JSON 配置文件。
;;
;; 语法
;; ----
;; (pi-read-json-file file-path) -> any | #f
;;
;; 参数
;; ----
;; file-path : string
;; JSON 文件路径。
;;
;; 返回值
;; ----
;; any | #f
;; 解析结果；文件不存在或解析失败时返回 #f。

(import (liii check) (liii pi code config settings))

(check-set-mode! 'report-failed)
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
