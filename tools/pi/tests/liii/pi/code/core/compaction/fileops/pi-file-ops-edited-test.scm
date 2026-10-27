(import (scheme base)
  (liii check)
  (liii pi code core compaction fileops))

(check-set-mode! 'report-failed)

;; pi-file-ops-edited
;; 返回 edit 工具编辑过的文件路径。
;;
;; 语法
;; ----
;; (pi-file-ops-edited ops)
;;
;; 参数
;; ----
;; ops : pi-file-ops
;; 文件操作分组 record。
;;
;; 返回值
;; ----
;; list
;; 去重后的编辑路径列表。
;;
;; 示例
;; ----
;; (pi-file-ops-edited ops) => '("c.scm")

(check (pi-file-ops-edited (make-pi-file-ops '("a.scm") '("b.scm") '("c.scm"))) => '("c.scm"))

(check-report)
