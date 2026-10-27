(import (scheme base)
  (liii check)
  (liii pi code core compaction fileops))

(check-set-mode! 'report-failed)

;; pi-file-ops-read
;; 返回 read 工具读取过的文件路径。
;;
;; 语法
;; ----
;; (pi-file-ops-read ops)
;;
;; 参数
;; ----
;; ops : pi-file-ops
;; 文件操作分组 record。
;;
;; 返回值
;; ----
;; list
;; 去重后的读取路径列表。
;;
;; 示例
;; ----
;; (pi-file-ops-read ops) => '("a.scm")

(check (pi-file-ops-read (make-pi-file-ops '("a.scm") '("b.scm") '("c.scm"))) => '("a.scm"))
(check (pi-file-ops-read (make-pi-file-ops '() '() '())) => '())

(check-report)
