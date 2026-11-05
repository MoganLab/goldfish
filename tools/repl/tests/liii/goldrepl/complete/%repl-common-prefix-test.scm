;; %repl-common-prefix
;; 计算字符串列表的最长公共前缀。
;;
;; 语法
;; ----
;; (%repl-common-prefix strs)
;;
;; 参数
;; ----
;; strs : list
;; 字符串列表。
;;
;; 返回值
;; ----
;; string
;; 所有字符串的最长公共前缀；列表为空时返回空字符串。

(import (scheme base) (liii check) (liii list) (liii goldrepl complete))

(check-set-mode! 'report-failed)

;; 空列表
(check (%repl-common-prefix '()) => "")

;; 单个字符串
(check (%repl-common-prefix '("single")) => "single")

;; 多个字符串
(check (%repl-common-prefix '("string-append" "string-copy" "string-map"))
  =>
  "string-"
) ;check

;; 没有公共前缀
(check (%repl-common-prefix '("apple" "banana")) => "")

(check-report)
