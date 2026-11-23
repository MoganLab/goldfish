;; gfproject-package-description
;; 返回 gfproject package 的简短描述。
;;
;; 语法
;; ----
;; (gfproject-package-description package)
;;
;; 参数
;; ----
;; package : gfproject-package?
;; 包元数据记录。
;;
;; 返回值
;; ----
;; string
;; 包描述；构造时未提供则为空字符串。

(import (liii check) (liii gfproject package))

(check-set-mode! 'report-failed)
(check (gfproject-package-description (gfproject-package "lang" "liii" "Goldfish")) => "Goldfish")
(check (gfproject-package-description (gfproject-package "lang" "liii")) => "")

(check-report)
