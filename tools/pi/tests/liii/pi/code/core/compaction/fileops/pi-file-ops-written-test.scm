(import (scheme base)
  (liii check)
  (liii pi code core compaction fileops))

(check-set-mode! 'report-failed)

;; pi-file-ops-written
;; 返回 write 工具写入过的文件路径。
;;
;; 语法
;; ----
;; (pi-file-ops-written ops)
;;
;; 参数
;; ----
;; ops : pi-file-ops
;; 文件操作分组 record。
;;
;; 返回值
;; ----
;; list
;; 去重后的写入路径列表。
;;
;; 示例
;; ----
;; (pi-file-ops-written ops) => '("b.scm")

(check (pi-file-ops-written (make-pi-file-ops '("a.scm") '("b.scm") '("c.scm"))) => '("b.scm"))

(check-report)
