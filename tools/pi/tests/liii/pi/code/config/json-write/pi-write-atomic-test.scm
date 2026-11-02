;; pi-write-atomic
;; 原子写入单个文件：先写临时文件，再替换目标文件。
;;
;; 语法
;; ----
;; (pi-write-atomic abs-path text)
;;
;; 参数
;; ----
;; abs-path : string
;; 目标文件的绝对路径。
;; text : string
;; 要写入的文本。
;;
;; 返回值
;; ----
;; undefined
;; 写入完成后返回未定义值。

(import (liii check) (liii path) (liii shutil) (liii pi code config json-write))

(check-set-mode! 'report-failed)

(define DIR (path->string (path-join (path-temp-dir) "pi-write-atomic-test")))
(define FILE (path->string (path-join (path DIR) "config.json")))

(when (file-exists? DIR)
  (rmtree DIR #t))
(path-mkdir DIR :parents #t :exist-ok #t)

;; 目标不存在时直接创建
(pi-write-atomic FILE "{\"a\": 1}\n")
(check (path-read-text FILE) => "{\"a\": 1}\n")

;; 目标已存在时替换内容
(pi-write-atomic FILE "{\"a\": 2}\n")
(check (path-read-text FILE) => "{\"a\": 2}\n")

;; 不残留临时文件
(check (file-exists? (string-append FILE ".tmp")) => #f)
(check (file-exists? (string-append FILE ".bak")) => #f)

(rmtree DIR #t)

(check-report)
