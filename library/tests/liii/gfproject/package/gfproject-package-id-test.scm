;; gfproject-package-id
;; 返回 gfproject package 的 name@organization 标识。
;;
;; 语法
;; ----
;; (gfproject-package-id package)
;;
;; 参数
;; ----
;; package : gfproject-package?
;; 包元数据记录。
;;
;; 返回值
;; ----
;; string
;; name@organization 形式的包标识。

(import (liii check) (liii gfproject package))

(check-set-mode! 'report-failed)
(check (gfproject-package-id (gfproject-package "lang" "liii")) => "lang@liii")
(check (gfproject-package-id (gfproject-package "pi" "liii")) => "pi@liii")
(check-catch 'type-error (gfproject-package-id '(("name" . "lang"))))

(check-report)
