;; delimiter-char?
;; 判断字符是否为分隔符：空白字符、左括号、右括号、分号与双引号。
;;
;; 语法
;; ----
;; (delimiter-char? c)
;;
;; 参数
;; ----
;; c : character? 待判断的字符
;;
;; 返回值
;; ------
;; boolean? 是分隔符时为 #t，否则为 #f

(import (liii check) (liii base) (liii goldfix scan))

(check-set-mode! 'report-failed)

;; 双引号字符：gf fmt 的分词器暂不支持 #\x 字符字面量，故由整数构造

(define double-quote (integer->char 34))

;; 测试：空白字符也是分隔符
(check (delimiter-char? #\space) => #t)
(check (delimiter-char? #\newline) => #t)

;; 测试：括号、分号与双引号是分隔符
(check (delimiter-char? #\() => #t)
(check (delimiter-char? #\)) => #t)
(check (delimiter-char? #\;) => #t)
(check (delimiter-char? double-quote) => #t)

;; 测试：普通字符不是分隔符
(check (delimiter-char? #\a) => #f)
(check (delimiter-char? #\#) => #f)
(check (delimiter-char? #\|) => #f)

(check-report)
