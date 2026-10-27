(import (scheme base)
  (liii check)
  (liii pi code core compaction fileops))

(check-set-mode! 'report-failed)

;; pi-file-lists-modified-files
;; 返回写入或编辑过的文件路径列表。
;;
;; 语法
;; ----
;; (pi-file-lists-modified-files lists)
;;
;; 参数
;; ----
;; lists : pi-file-lists
;; 文件列表 record。
;;
;; 返回值
;; ----
;; list
;; 排序并去重后的已修改文件路径列表。
;;
;; 示例
;; ----
;; (pi-file-lists-modified-files lists) => '("b.scm" "c.scm")

(check (pi-file-lists-modified-files
         (make-pi-file-lists '("a.scm") '("b.scm" "c.scm"))) => '("b.scm" "c.scm"))

(check-report)
