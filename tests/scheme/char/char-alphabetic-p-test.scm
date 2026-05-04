(import (liii check) (scheme char))
(check-set-mode! 'report-failed)

;; char-alphabetic?
;; 判断字符是否为字母字符。
;;
;; 语法
;; ----
;; (char-alphabetic? char) -> boolean?
;;
;; 参数
;; ----
;; char : character
;; 要判断的字符
;;
;; 返回值
;; ------
;; boolean?
;; 如果字符是字母则返回 #t，否则返回 #f
;;
;; 注意
;; ----
;; - 对于字母字符（A-Z, a-z 及 Unicode 字母），返回 #t
;; - 对于非字母字符，返回 #f
;; - 当前实现基于 Chez Scheme 10.3 的 char-alphabetic? 行为
;; - 遵循 R7RS 标准规范
;;
;; 错误处理
;; ------
;; type-error
;; 参数必须是字符类型，否则会抛出异常

;; ASCII 字母测试
(check (char-alphabetic? #\x0041) => #t)
(check (char-alphabetic? #\x005A) => #t)
(check (char-alphabetic? #\x0061) => #t)
(check (char-alphabetic? #\x007A) => #t)

;; Unicode 字母字符测试（基于 Chez Scheme 10.3）
(check (char-alphabetic? #\x00AA) => #t)
(check (char-alphabetic? #\x00B5) => #t)
(check (char-alphabetic? #\x00C0) => #t)
(check (char-alphabetic? #\x00D6) => #t)
(check (char-alphabetic? #\x00D8) => #t)
(check (char-alphabetic? #\x00F6) => #t)
(check (char-alphabetic? #\x00F8) => #t)
(check (char-alphabetic? #\x00FF) => #t)
(check (char-alphabetic? #\x0391) => #t)
(check (char-alphabetic? #\x03A9) => #t)
(check (char-alphabetic? #\x03B1) => #t)
(check (char-alphabetic? #\x03C9) => #t)
(check (char-alphabetic? #\x0410) => #t)
(check (char-alphabetic? #\x042F) => #t)
(check (char-alphabetic? #\x0430) => #t)
(check (char-alphabetic? #\x044F) => #t)
(check (char-alphabetic? #\x4E00) => #t)
(check (char-alphabetic? #\x4E2D) => #t)
(check (char-alphabetic? #\x9FFF) => #t)
(check (char-alphabetic? #\xAC00) => #t)
(check (char-alphabetic? #\xD7A3) => #t)
(check (char-alphabetic? #\x05D0) => #t)
(check (char-alphabetic? #\x05EA) => #t)
(check (char-alphabetic? #\x0627) => #t)
(check (char-alphabetic? #\x064A) => #t)
(check (char-alphabetic? #\x3042) => #t)
(check (char-alphabetic? #\x3093) => #t)
(check (char-alphabetic? #\x30A2) => #t)
(check (char-alphabetic? #\x30F3) => #t)

;; 非字母字符测试
(check (char-alphabetic? #\x0030) => #f)
(check (char-alphabetic? #\x0039) => #f)
(check (char-alphabetic? #\x0020) => #f)
(check (char-alphabetic? #\x0021) => #f)
(check (char-alphabetic? #\x00A0) => #f)
(check (char-alphabetic? #\x0300) => #f)
(check (char-alphabetic? #\x2600) => #f)
(check (char-alphabetic? #\x2605) => #f)

;; 错误处理测试
(check-catch 'type-error (char-alphabetic? 1))
(check-catch 'type-error (char-alphabetic? "a"))
(check-catch 'wrong-number-of-args (char-alphabetic?))
(check-catch 'wrong-number-of-args (char-alphabetic? #\a #\b))

(check-report)
