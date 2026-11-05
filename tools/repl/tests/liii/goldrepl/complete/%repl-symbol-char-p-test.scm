;; %repl-symbol-char?
;; 判断字符是否可以出现在符号名中。
;;
;; 语法
;; ----
;; (%repl-symbol-char? ch)
;;
;; 参数
;; ----
;; ch : char
;; 待判断的字符。
;;
;; 返回值
;; ----
;; boolean
;; 字符可以出现在符号名中时返回真值，否则返回 #f。

(import (scheme base) (liii check) (liii goldrepl complete))

(check-set-mode! 'report-failed)

;; %repl-symbol-char? 对特殊符号字符返回的是真值而不是 #t

(define (truthy? value)
  (if value #t #f)
) ;define

;; 字母属于符号字符
(check (%repl-symbol-char? #\a) => #t)
(check (%repl-symbol-char? #\Z) => #t)

;; 数字与空白不属于符号字符
(check (%repl-symbol-char? #\1) => #f)
(check (%repl-symbol-char? #\space) => #f)

;; 括号不属于符号字符
(check (%repl-symbol-char? #\() => #f)
(check (%repl-symbol-char? #\)) => #f)

;; 常见符号字符
(check (truthy? (%repl-symbol-char? #\!)) => #t)
(check (truthy? (%repl-symbol-char? #\$)) => #t)
(check (truthy? (%repl-symbol-char? #\%)) => #t)
(check (truthy? (%repl-symbol-char? #\&)) => #t)
(check (truthy? (%repl-symbol-char? #\*)) => #t)
(check (truthy? (%repl-symbol-char? #\/)) => #t)
(check (truthy? (%repl-symbol-char? #\:)) => #t)
(check (truthy? (%repl-symbol-char? #\<)) => #t)
(check (truthy? (%repl-symbol-char? #\=)) => #t)
(check (truthy? (%repl-symbol-char? #\>)) => #t)
(check (truthy? (%repl-symbol-char? #\?)) => #t)
(check (truthy? (%repl-symbol-char? #\^)) => #t)
(check (truthy? (%repl-symbol-char? #\_)) => #t)
(check (truthy? (%repl-symbol-char? #\~)) => #t)
(check (truthy? (%repl-symbol-char? #\+)) => #t)
(check (truthy? (%repl-symbol-char? #\-)) => #t)
(check (truthy? (%repl-symbol-char? #\.)) => #t)

(check-report)
