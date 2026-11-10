;; make-error-expected
;; 构造“期望某记号”类型的解析错误。
;;
;; 语法
;; ----
;; (make-error-expected pos str)
;;
;; 参数
;; ----
;; pos : <parse-position> 或 #f
;; 出错位置。
;; str : string
;; 期望的记号描述。
;;
;; 返回值
;; ----
;; <parse-error>
;; 解析错误记录，其 expected 字段为单元素列表。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(let ((pos (make-parse-position "test.scm" 2 10)))
  (let ((error-ex (make-error-expected pos "open-paren")))
    (check-true (parse-error? error-ex))
    (check (parse-error-position error-ex) => pos)
    (check (parse-error-expected error-ex) => '("open-paren"))
    (check (parse-error-messages error-ex) => '())
  ) ;let
) ;let

(check-report)
