;; pi-user-read-json
;; 读取 JSON 配置文件并解析为数据。
;;
;; 语法
;; ----
;; (pi-user-read-json filepath)
;;
;; 参数
;; ----
;; filepath : (or string path)
;; JSON 文件路径。
;;
;; 返回值
;; ----
;; any
;; 解析后的数据，文件缺失或语法错误时返回 #f。

(import (liii check)
  (liii path)
  (liii pi code config user-fs)
) ;import

(check-set-mode! 'report-failed)

(define tmp-file
  (path-join (path-temp-dir) "pi-user-read-json-test.json")
) ;define

(when (path-exists? tmp-file) (path-unlink tmp-file))

;; 1. 文件缺失时返回 #f
(check (pi-user-read-json (path->string tmp-file)) => #f)

;; 2. 正常解析 JSON 内容
(path-write-text tmp-file "{\"name\":\"kimi\",\"enabled\":true}")
(let ((data (pi-user-read-json (path->string tmp-file))))
  (check (cdr (assoc "name" data)) => "kimi")
) ;let*

;; 3. 语法错误返回 #f
(path-write-text tmp-file "{not json")
(check (pi-user-read-json (path->string tmp-file)) => #f)

(when (path-exists? tmp-file) (path-unlink tmp-file))

(check-report)
