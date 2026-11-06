;; parse-error-expected
;; 读取解析错误中期望的记号集合。
;;
;; 语法
;; ----
;; (parse-error-expected err)
;;
;; 参数
;; ----
;; err : <parse-error>
;; 解析错误记录。
;;
;; 返回值
;; ----
;; list
;; 期望记号的字符串列表；没有期望记号时为空列表。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(let ((pos (make-parse-position "test.scm" 2 10)))
  (check (parse-error-expected (make-error-expected pos "open-paren")) => '("open-paren"))
  (check (parse-error-expected (make-error-message pos "syntax error")) => '())
) ;let

(check-report)
