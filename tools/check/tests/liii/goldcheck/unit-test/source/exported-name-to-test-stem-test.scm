;; exported-name->test-stem
;; 将 Scheme 导出过程名转换为符合约定的测试文件名前缀（主干）。
;;
;; 语法
;; ----
;; (exported-name->test-stem name)
;;
;; 参数
;; ----
;; name : string
;; Scheme 过程或导出标识符的字符串名称。
;;
;; 返回值
;; ------
;; string
;; 转换后的测试文件名主干。
;;
;; 说明
;; ----
;; 特殊字符转换规则：
;; 1. 纯运算符：+ -> plus, - -> minus, * -> star, / -> slash, = -> eq, < -> lt 等
;; 2. 尾缀转换：? -> -p, ! -> -bang, -> -> -to 或 -to-, / -> -slash 或 -slash- 等
;; 3. Windows 非法字符：: -> -colon

(import (scheme base) (liii check) (liii goldcheck unit-test source))

(check-set-mode! 'report-failed)

;; 纯运算符
(check (exported-name->test-stem "+") => "plus")
(check (exported-name->test-stem "-") => "minus")
(check (exported-name->test-stem "*") => "star")
(check (exported-name->test-stem "/") => "slash")
(check (exported-name->test-stem "=") => "eq")
(check (exported-name->test-stem "<") => "lt")
(check (exported-name->test-stem "<=") => "le")
(check (exported-name->test-stem ">") => "gt")
(check (exported-name->test-stem ">=") => "ge")

;; 常见符号转换
(check (exported-name->test-stem "null?") => "null-p")
(check (exported-name->test-stem "set-car!") => "set-car-bang")
(check (exported-name->test-stem "string->utf8") => "string-to-utf8")
(check (exported-name->test-stem "char=?") => "char-eq-p")
(check (exported-name->test-stem "truncate/") => "truncate-slash")
(check (exported-name->test-stem "tz-month-week-day:month") => "tz-month-week-day-colon-month")

(check-report)
