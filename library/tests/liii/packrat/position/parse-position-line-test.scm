;; parse-position-line
;; 读取解析位置记录的行号。
;;
;; 语法
;; ----
;; (parse-position-line pos)
;;
;; 参数
;; ----
;; pos : <parse-position>
;; 解析位置记录。
;;
;; 返回值
;; ----
;; exact-integer?
;; 行号，从 1 开始。

(import (liii check) (liii packrat position))

(check-set-mode! 'report-failed)

(check (parse-position-line (make-parse-position "test.scm" 3 15)) => 3)
(check (parse-position-line (make-parse-position "test.scm" 1 0)) => 1)

(check-report)
