(import (liii check)
        (scheme char)
) ;import

(check-set-mode! 'report-failed)

;; digit-value
;; 获取数字字符的数值
;;
;; 语法
;; ----
;; (digit-value char) → integer | #f
;;
;; 参数
;; ----
;; char : character
;; 要获取数值的字符
;;
;; 返回值
;; ------
;; integer | #f
;; 如果字符是数字字符，返回对应的整数值（0-9）；否则返回 #f
;;
;; 注意
;; ----
;; - 对于数字字符 #\0 到 #\9，返回对应的整数值 0 到 9
;; - 对于非数字字符，返回 #f
;; - 遵循 R7RS 标准规范
;;
;; 错误处理
;; ------
;; type-error
;; 参数必须是字符类型，否则会抛出异常

;; 数字字符测试
(check (digit-value #\0) => 0)
(check (digit-value #\1) => 1)
(check (digit-value #\2) => 2)
(check (digit-value #\3) => 3)
(check (digit-value #\4) => 4)
(check (digit-value #\5) => 5)
(check (digit-value #\6) => 6)
(check (digit-value #\7) => 7)
(check (digit-value #\8) => 8)
(check (digit-value #\9) => 9)

;; 非数字字符测试
(check (digit-value #\a) => #f)
(check (digit-value #\c) => #f)
(check (digit-value #\A) => #f)
(check (digit-value #\Z) => #f)
(check (digit-value #\space) => #f)
(check (digit-value #\newline) => #f)
(check (digit-value #\null) => #f)
(check (digit-value #\.) => #f)
(check (digit-value #\,) => #f)
(check (digit-value #\!) => #f)
(check (digit-value #\@) => #f)
(check (digit-value #\$) => #f)
(check (digit-value #\%) => #f)
(check (digit-value #\^) => #f)
(check (digit-value #\&) => #f)
(check (digit-value #\*) => #f)
(check (digit-value #\() => #f)
(check (digit-value #\)) => #f)
(check (digit-value #\_) => #f)
(check (digit-value #\+) => #f)
(check (digit-value #\-) => #f)
(check (digit-value #\=) => #f)
(check (digit-value #\[) => #f)
(check (digit-value #\]) => #f)
(check (digit-value #\{) => #f)
(check (digit-value #\}) => #f)
(check (digit-value #\|) => #f)
(check (digit-value #\\) => #f)
(check (digit-value #\:) => #f)
(check (digit-value #\;) => #f)
(check (digit-value #\") => #f)
(check (digit-value #\') => #f)
(check (digit-value #\<) => #f)
(check (digit-value #\>) => #f)
(check (digit-value #\?) => #f)
(check (digit-value #\/) => #f)

(check-report)
