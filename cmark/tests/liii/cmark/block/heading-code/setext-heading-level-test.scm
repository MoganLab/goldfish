;; setext-heading-level
;; 根据 Setext 下划线返回标题级别 1 或 2。
;;
;; 语法
;; ----
;; (setext-heading-level line)
;;
;; 参数
;; ----
;; line : string
;; 下划线行。
;;
;; 返回值
;; ----
;; integer
;; 返回 1 或 2。

(import (scheme base))
(import (liii check))
(import (liii cmark block heading-code))

(check-set-mode! 'report-failed)

(check (setext-heading-level "===") => 1)
(check (setext-heading-level "---") => 2)

(check-report)
