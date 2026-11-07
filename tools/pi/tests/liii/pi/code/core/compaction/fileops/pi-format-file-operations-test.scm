;; pi-format-file-operations
;; 将文件操作汇总格式化为可读文本。
;;
;; 语法
;; ----
;; (pi-format-file-operations read-files modified-files)
;;
;; 参数
;; ----
;; read-files : list
;; 被读取的文件路径列表。
;;
;; modified-files : list
;; 被修改的文件路径列表。
;;
;; 返回值
;; ----
;; string
;; 格式化后的文件操作文本。

(import (liii check) (liii string) (liii pi code core compaction fileops))

(check-set-mode! 'report-failed)

(let ((text (pi-format-file-operations '("/a") '("/b"))))
  (check-true (string-contains? text "<read-files>\n/a\n</read-files>"))
  (check-true (string-contains? text "<modified-files>\n/b\n</modified-files>"))
) ;let

;; 空列表不产生段落
(check (pi-format-file-operations '() '()) => "")

(check-report)
