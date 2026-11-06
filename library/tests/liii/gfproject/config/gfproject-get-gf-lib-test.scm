;; gfproject-get-gf-lib
;; 返回 Goldfish 标准库目录路径。
;;
;; 语法
;; ----
;; (gfproject-get-gf-lib)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; string
;; Goldfish 库目录的绝对路径。
;;
;; 说明
;; ----
;; 该目录是 (liii gfproject) 查找 gfproject.json 的默认位置。

(import (liii check) (liii gfproject config))

(check-set-mode! 'report-failed)

(let ((lib (gfproject-get-gf-lib)))
  (check-true (string? lib))
  (check-true (> (string-length lib) 0))
) ;let

(check-report)
