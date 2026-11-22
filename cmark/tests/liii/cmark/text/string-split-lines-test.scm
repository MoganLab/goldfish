;; string-split-lines
;; 按行切分字符串，自动剥除行尾 CR 并丢弃末尾空行。
;;
;; 语法
;; ----
;; (string-split-lines str)
;;
;; 参数
;; ----
;; str : string
;; 输入文本。
;;
;; 返回值
;; ----
;; list
;; 返回行字符串列表。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check (string-split-lines "a\nb\n") => '("a" "b"))
(check (string-split-lines "a\r\nb") => '("a" "b"))
(check (string-split-lines "") => '())

(check-report)
