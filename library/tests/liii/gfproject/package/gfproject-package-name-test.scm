;; gfproject-package-name
;; 返回 gfproject package 的包名。
;;
;; 语法
;; ----
;; (gfproject-package-name package)
;;
;; 参数
;; ----
;; package : gfproject-package?
;; 包元数据记录。
;;
;; 返回值
;; ----
;; string
;; 包名。

(import (liii check) (liii gfproject package))

(check-set-mode! 'report-failed)
(check (gfproject-package-name (gfproject-package "lang" "liii")) => "lang")

(check-report)
