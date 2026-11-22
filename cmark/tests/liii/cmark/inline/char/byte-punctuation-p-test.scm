;; byte-punctuation?
;; 判断 ASCII 字符是否为标点符号。
;;
;; 语法
;; ----
;; (byte-punctuation? c)
;;
;; 参数
;; ----
;; c : char
;; 输入字符。
;;
;; 返回值
;; ----
;; boolean
;; 是标点返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline char))

(check-set-mode! 'report-failed)

(check-true (byte-punctuation? #\*))
(check-false (byte-punctuation? #\a))

(check-report)
