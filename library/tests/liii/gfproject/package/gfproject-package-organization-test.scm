;; gfproject-package-organization
;; 返回 gfproject package 的发布组织。
;;
;; 语法
;; ----
;; (gfproject-package-organization package)
;;
;; 参数
;; ----
;; package : gfproject-package?
;; 包元数据记录。
;;
;; 返回值
;; ----
;; string
;; 发布组织。

(import (liii check) (liii gfproject package))

(check-set-mode! 'report-failed)
(check (gfproject-package-organization (gfproject-package "lang" "liii")) => "liii")

(check-report)
