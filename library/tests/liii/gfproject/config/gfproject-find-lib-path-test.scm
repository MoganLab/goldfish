;; gfproject-find-lib-path
;; 查找标准库目录中的 gfproject.json 路径。
;;
;; 语法
;; ----
;; (gfproject-find-lib-path)
;; (gfproject-find-lib-path gf-lib)
;;
;; 参数
;; ----
;; gf-lib : string (可选)
;; 标准库目录路径；缺省时使用默认标准库目录。
;;
;; 返回值
;; ----
;; string 或 #f
;; gfproject.json 的路径；库目录及其上级目录都没有该文件时为 #f。
;;
;; 说明
;; ----
;; 先查 <库目录>/gfproject.json，再查 <库目录的上一级>/gfproject.json。

(import (liii check) (liii gfproject config) (liii string))

(check-set-mode! 'report-failed)

;; 默认库目录
(let ((path (gfproject-find-lib-path)))
  (if path (check-true (string-ends? path "gfproject.json")) (check-true #t))
) ;let

;; 指定不存在的库目录时返回 #f
(check-false (gfproject-find-lib-path "/no/such/gf-lib-dir"))

(check-report)
