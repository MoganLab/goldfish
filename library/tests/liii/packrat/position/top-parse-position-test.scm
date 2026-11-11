;; top-parse-position
;; 构造文件起始位置的解析位置记录。
;;
;; 语法
;; ----
;; (top-parse-position filename)
;;
;; 参数
;; ----
;; filename : string
;; 源文件名。
;;
;; 返回值
;; ----
;; <parse-position>
;; 行号为 1、列号为 0 的解析位置记录。

(import (liii check) (liii packrat position))

(check-set-mode! 'report-failed)

(let ((pos (top-parse-position "test.scm")))
  (check-true (parse-position? pos))
  (check (parse-position-file pos) => "test.scm")
  (check (parse-position-line pos) => 1)
  (check (parse-position-column pos) => 0)
) ;let

(check-report)
