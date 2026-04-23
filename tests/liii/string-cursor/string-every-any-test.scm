(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-every: 检查是否所有字符都满足谓词
;; 空字符串返回 #t
(check (string-every char? "") => #t)

;; 所有字符都满足
(check (string-every char? "abc") => #t)
(check (string-every char-alphabetic? "abc") => #t)

;; 不是所有字符都满足
(check (string-every char-alphabetic? "abc123") => #f)

;; 返回最后一个满足谓词的结果
(check (string-every (lambda (c) (if (char-alphabetic? c) c #f)) "abc") => #\c)

;; 测试中文字符
(check (string-every char? "中文") => #t)
(check (string-every (lambda (c) (if (char>? c #\a) c #f)) "bcd") => #\d)

;; string-any: 检查是否有任意字符满足谓词
;; 空字符串返回 #f
(check (string-any char? "") => #f)

;; 有字符满足
(check (string-any char-numeric? "abc123") => #t)
(check (string-any char-numeric? "123") => #t)

;; 没有字符满足
(check (string-any char-numeric? "abc") => #f)

;; 返回第一个满足谓词的结果
(check (string-any (lambda (c) (if (char-numeric? c) c #f)) "abc123") => #\1)

;; 测试中文字符
(check (string-any char? "中文") => #t)

(check-report)
