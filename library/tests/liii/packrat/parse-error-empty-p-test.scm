;; parse-error-empty?
;; 判断解析错误是否既无期望记号也无错误消息。
;;
;; 语法
;; ----
;; (parse-error-empty? err)
;;
;; 参数
;; ----
;; err : <parse-error>
;; 解析错误记录。
;;
;; 返回值
;; ----
;; boolean
;; 错误为空时返回 #t，否则返回 #f。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(let ((pos (make-parse-position "test.scm" 2 10)))
  (check-false (parse-error-empty? (make-error-expected pos "open-paren")))
  (check-false (parse-error-empty? (make-error-message pos "boom")))
) ;let

(check-report)
