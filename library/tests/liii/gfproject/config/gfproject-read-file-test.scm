;; gfproject-read-file
;; 读取 JSON 配置文件内容。
;;
;; 语法
;; ----
;; (gfproject-read-file path)
;;
;; 参数
;; ----
;; path : string 或 #f
;; JSON 文件路径。
;;
;; 返回值
;; ----
;; list
;; 解析后的 JSON 对象（形如 (("key" . value) ...)）；
;; 文件不存在、解析失败或内容不是对象时返回空对象 (())。

(import (liii check) (liii gfproject config) (liii json) (liii os))

(check-set-mode! 'report-failed)

;; 文件不存在
(check (gfproject-read-file "/no/such/config-file.json") => '(()))
(check (gfproject-read-file #f) => '(()))

;; 正常读取
(let* ((path (string-append (os-temp-dir) "/gfproject-read-file-test.json"))
       (port (open-output-file path "w"))
      ) ;
  (display "{\"a\": 1}" port)
  (close-output-port port)
  (let ((data (gfproject-read-file path)))
    (check-true (json-object? data))
    (check (json-ref data "a") => 1)
  ) ;let
  (delete-file path)
) ;let*

;; 内容不是 JSON 对象
(let* ((path (string-append (os-temp-dir) "/gfproject-read-file-bad.json"))
       (port (open-output-file path "w"))
      ) ;
  (display "[1, 2]" port)
  (close-output-port port)
  (check (gfproject-read-file path) => '(()))
  (delete-file path)
) ;let*

(check-report)
