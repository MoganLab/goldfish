;; gfproject-package-version
;; 返回 gfproject package 的版本字符串。
;;
;; 语法
;; ----
;; (gfproject-package-version package)
;;
;; 参数
;; ----
;; package : gfproject-package?
;; 包元数据记录。
;;
;; 返回值
;; ----
;; (or #f string)
;; 包版本字符串；构造时未提供则为 #f。

(import (liii check) (liii gfproject package))

(check-set-mode! 'report-failed)
(check (gfproject-package-version (gfproject-package "lang" "liii" "Goldfish" "2026.11.24")
       ) ;gfproject-package-version
  =>
  "2026.11.24"
) ;check
(check (gfproject-package-version (gfproject-package "lang" "liii")) => #f)
(check (gfproject-package-version (gfproject-package "pi" "liii" "Pi coding agent"))
  =>
  #f
) ;check

(check-report)
