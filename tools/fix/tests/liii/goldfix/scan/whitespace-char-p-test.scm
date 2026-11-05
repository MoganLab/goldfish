;; whitespace-char?
;; 判断字符是否为空白字符：空格、制表符、换行符与回车符。
;;
;; 语法
;; ----
;; (whitespace-char? c)
;;
;; 参数
;; ----
;; c : character? 待判断的字符
;;
;; 返回值
;; ------
;; boolean? 是空白字符时为 #t，否则为 #f

(import (liii check) (liii base) (liii goldfix scan))

(check-set-mode! 'report-failed)

;; 测试：空白字符
(check (whitespace-char? #\space) => #t)
(check (whitespace-char? #\tab) => #t)
(check (whitespace-char? #\newline) => #t)
(check (whitespace-char? #\return) => #t)

;; 测试：非空白字符
(check (whitespace-char? #\a) => #f)
(check (whitespace-char? #\() => #f)
(check (whitespace-char? #\)) => #f)
(check (whitespace-char? #\;) => #f)
(check (whitespace-char? #\") => #f)
(check-report)
