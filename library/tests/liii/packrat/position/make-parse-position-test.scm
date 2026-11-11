;; make-parse-position
;; 构造解析位置记录。
;;
;; 语法
;; ----
;; (make-parse-position file line column)
;;
;; 参数
;; ----
;; file : string 或 #f
;; 源文件名，未知时为 #f。
;; line : exact-integer?
;; 行号，从 1 开始。
;; column : exact-integer?
;; 列号，从 0 开始。
;;
;; 返回值
;; ----
;; <parse-position>
;; 解析位置记录。
;;
;; 说明
;; ----
;; 解析位置用于在解析错误中定位到具体文件行列。

(import (liii check) (liii packrat position))

(check-set-mode! 'report-failed)

(let ((pos (make-parse-position "test.scm" 3 15)))
  (check-true (parse-position? pos))
  (check (parse-position-file pos) => "test.scm")
  (check (parse-position-line pos) => 3)
  (check (parse-position-column pos) => 15)
) ;let

;; 未知文件名
(let ((pos (make-parse-position #f 1 0)))
  (check-true (parse-position? pos))
  (check-false (parse-position-file pos))
) ;let

(check-report)
