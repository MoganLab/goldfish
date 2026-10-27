(import (scheme base)
  (liii check)
  (liii pi code core compaction fileops))

(check-set-mode! 'report-failed)

;; make-pi-file-lists
;; 构造压缩摘要使用的只读与已修改文件列表 record。
;;
;; 语法
;; ----
;; (make-pi-file-lists read-files modified-files)
;;
;; 参数
;; ----
;; read-files : list
;; 仅读取而未修改的文件路径列表。
;; modified-files : list
;; 写入或编辑过的文件路径列表。
;;
;; 返回值
;; ----
;; pi-file-lists
;; 文件列表 record。
;;
;; 示例
;; ----
;; (pi-file-lists-read-files
;;   (make-pi-file-lists '("a.scm") '("b.scm"))) => '("a.scm")

(let ((lists (make-pi-file-lists '("a.scm") '("b.scm" "c.scm"))))
  (check-true (pi-file-lists? lists))
  (check (pi-file-lists-read-files lists) => '("a.scm"))
  (check (pi-file-lists-modified-files lists) => '("b.scm" "c.scm")))

(check-report)
