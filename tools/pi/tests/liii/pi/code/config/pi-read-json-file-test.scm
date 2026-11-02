;; pi-read-json-file
;; 读取并解析 JSON 配置文件。
;;
;; 语法
;; ----
;; (pi-read-json-file filepath)
;;
;; 参数
;; ----
;; filepath : string
;; 要读取的 JSON 文件路径。
;;
;; 返回值
;; ----
;; alist | #f
;; 解析后的 JSON 对象；文件不存在时返回 #f。

(import (liii check) (liii pi code config))

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
