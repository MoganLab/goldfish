;; first-positional
;; 取出参数解析器中的第一个位置参数，没有位置参数时返回空字符串。
;;
;; 语法
;; ----
;; (first-positional parser)
;;
;; 参数
;; ----
;; parser : procedure? 参数解析器过程
;;
;; 返回值
;; ------
;; string? 第一个位置参数，不存在时为空字符串

(import (liii check) (liii base) (liii goldfix cli))

(check-set-mode! 'report-failed)

;; 测试：没有位置参数时返回空字符串
(let ((parser (make-fix-arg-parser)))
  (parser :parse-args '("fix"))
  (check (first-positional parser) => "")
) ;let

;; 测试：返回第一个位置参数
(let ((parser (make-fix-arg-parser)))
  (parser :parse-args '("fix" "a.scm" "b.scm"))
  (check (first-positional parser) => "a.scm")
) ;let

(check-report)
