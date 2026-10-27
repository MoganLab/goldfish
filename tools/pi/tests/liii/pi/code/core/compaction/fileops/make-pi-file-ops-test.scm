(import (scheme base)
  (liii check)
  (liii pi code core compaction fileops))

(check-set-mode! 'report-failed)

;; make-pi-file-ops
;; 构造按 read、write、edit 分类的文件操作 record。
;;
;; 语法
;; ----
;; (make-pi-file-ops read written edited)
;;
;; 参数
;; ----
;; read : list
;; 读取过的文件路径列表。
;; written : list
;; 写入过的文件路径列表。
;; edited : list
;; 编辑过的文件路径列表。
;;
;; 返回值
;; ----
;; pi-file-ops
;; 文件操作分组 record。
;;
;; 示例
;; ----
;; (pi-file-ops-read (make-pi-file-ops '("a.scm") '() '())) => '("a.scm")

(let ((ops (make-pi-file-ops '("a.scm") '("b.scm") '("c.scm"))))
  (check-true (pi-file-ops? ops))
  (check (pi-file-ops-read ops) => '("a.scm"))
  (check (pi-file-ops-written ops) => '("b.scm"))
  (check (pi-file-ops-edited ops) => '("c.scm")))

(check-report)
