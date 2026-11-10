;; (srfi srfi-166)
;; 兼容性总入口的冒烟测试：确认五个子库的导出都能经该库取到。
;;
;; 语法
;; ----
;; (import (srfi srfi-166))
;; (import (srfi 166))
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; 无。
;;
;; 说明
;; ----
;; 本库只是 base、color、columnar、pretty、unicode 的聚合再导出，
;; 这里每个子库只留一条代表性断言，具体函数行为在各自子目录测试。

(import (liii check)
  (srfi srfi-166)
  (srfi srfi-166 base)
  (srfi srfi-166 color)
  (srfi srfi-166 unicode)
  (srfi srfi-166 columnar)
  (srfi srfi-166 pretty)
  (srfi 166)
) ;import

(check-set-mode! 'report-failed)

(check (show #f "hello " "world") => "hello world")
(check (show #f (as-red "red")) => "\x1b;[31mred\x1b;[39m")
(check (show #f (on-blue "blue")) => "\x1b;[44mblue\x1b;[49m")
(check (show #f (upcased "abc")) => "ABC")
(check (show #f (downcased "ABC")) => "abc")
(check (show #f (padded 5 "abc")) => "  abc")
(check (show #f (trimmed 3 "abcde")) => "cde")
(check (show #f (fitted 5 "abc")) => "  abc")
(check (show #f (numeric 100)) => "100")
(check (show #f (columnar (each "*"))) => "*\n")
(check (show #f (pretty '(foo bar))) => "(foo bar)\n")

(check-report)
