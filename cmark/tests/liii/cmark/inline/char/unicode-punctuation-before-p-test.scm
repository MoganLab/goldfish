;; unicode-punctuation-before?
;; 判断 str 在 pos 之前是否为 Unicode 标点字符。
;;
;; 语法
;; ----
;; (unicode-punctuation-before? str pos)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; pos : integer
;; 字节索引。
;;
;; 返回值
;; ----
;; boolean
;; 前一位是标点返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline char))

(check-set-mode! 'report-failed)

(check-true (unicode-punctuation-before? "!a" 1))
(check-false (unicode-punctuation-before? "a!" 1))

(check-report)
