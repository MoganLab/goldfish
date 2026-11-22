;; html-tag-name-char?
;; 判断字符是否可作为 HTML 标签名字符。
;;
;; 语法
;; ----
;; (html-tag-name-char? ch)
;;
;; 参数
;; ----
;; ch : char
;; 待判断字符。
;;
;; 返回值
;; ----
;; boolean
;; 合法标签名字符返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark common))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (html-tag-name-char? #\a))
(check-true (html-tag-name-char? #\-))
(check-false (html-tag-name-char? #\space))

(check-report)
