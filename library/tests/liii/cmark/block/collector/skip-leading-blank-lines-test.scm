;; skip-leading-blank-lines
;; 跳过行列表开头的空白行。
;;
;; 语法
;; ----
;; (skip-leading-blank-lines lines)
;;
;; 参数
;; ----
;; lines : list
;; 行列表。
;;
;; 返回值
;; ----
;; list
;; 返回首个非空白行及后续行。

(import (scheme base))
(import (liii check))
(import (liii cmark block collector))

(check-set-mode! 'report-failed)

(check (skip-leading-blank-lines '("" "" "hi")) => '("hi"))

(check-report)
