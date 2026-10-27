(import (scheme base)
  (liii check)
  (liii pi code core compaction fileops))

(check-set-mode! 'report-failed)

;; pi-file-lists-read-files
;; 返回仅读取而未修改的文件路径列表。
;;
;; 语法
;; ----
;; (pi-file-lists-read-files lists)
;;
;; 参数
;; ----
;; lists : pi-file-lists
;; 文件列表 record。
;;
;; 返回值
;; ----
;; list
;; 排序后的只读文件路径列表。
;;
;; 示例
;; ----
;; (pi-file-lists-read-files lists) => '("a.scm")

(check (pi-file-lists-read-files (make-pi-file-lists '("a.scm") '("b.scm"))) => '("a.scm"))
(check (pi-file-lists-read-files (make-pi-file-lists '() '())) => '())

(check-report)
