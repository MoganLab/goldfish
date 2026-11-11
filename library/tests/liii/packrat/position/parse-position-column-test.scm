;; parse-position-column
;; 读取解析位置记录的列号。
;;
;; 语法
;; ----
;; (parse-position-column pos)
;;
;; 参数
;; ----
;; pos : <parse-position>
;; 解析位置记录。
;;
;; 返回值
;; ----
;; exact-integer?
;; 列号，从 0 开始。

(import (liii check) (liii packrat position))

(check-set-mode! 'report-failed)

(check (parse-position-column (make-parse-position "test.scm" 3 15)) => 15)
(check (parse-position-column (make-parse-position "test.scm" 1 0)) => 0)

(check-report)
