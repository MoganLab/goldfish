;; json-schema-split-on-char
;; 按指定字符把字符串切分成子串列表。
;;
;; 语法
;; ----
;; (json-schema-split-on-char value separator)
;;
;; 参数
;; ----
;; value : string
;; 待切分的字符串。
;;
;; separator : char
;; 切分用的分隔字符。
;;
;; 返回值
;; ----
;; list
;; 切分得到的子串列表，顺序与原串一致。
;;
;; 注意
;; ----
;; 分隔符连续出现时会产生空串片段，
;; 字符串末尾的分隔符也会产生末尾空串片段。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-helpers))

(check-set-mode! 'report-failed)

(check (json-schema-split-on-char "a/b/c" #\/) => '("a" "b" "c"))
(check (json-schema-split-on-char "a" #\/) => '("a"))
(check (json-schema-split-on-char "" #\/) => '(""))

;; 连续分隔符产生空串片段。

(check (json-schema-split-on-char "a//b" #\/) => '("a" "" "b"))
(check (json-schema-split-on-char "/a" #\/) => '("" "a"))
(check (json-schema-split-on-char "a/" #\/) => '("a" ""))

;; 分隔符可以不是斜杠。

(check (json-schema-split-on-char "a:b:c" #\:) => '("a" "b" "c"))

(check-report)
