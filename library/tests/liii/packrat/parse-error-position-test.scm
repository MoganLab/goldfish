;; parse-error-position
;; 读取解析错误的位置。
;;
;; 语法
;; ----
;; (parse-error-position err)
;;
;; 参数
;; ----
;; err : <parse-error>
;; 解析错误记录。
;;
;; 返回值
;; ----
;; <parse-position> 或 #f
;; 出错位置；未知时为 #f。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(let ((pos (make-parse-position "test.scm" 2 10)))
  (check (parse-error-position (make-error-expected pos "open-paren")) => pos)
  (check (parse-error-position (make-error-message pos "syntax error")) => pos)
) ;let

(check-report)
