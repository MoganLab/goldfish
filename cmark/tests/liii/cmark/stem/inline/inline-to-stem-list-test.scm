;; inline->stem-list
;; 将行内文本解析并转换为 md-stem 行内元素列表。
;;
;; 语法
;; ----
;; (inline->stem-list str . maybe-refmap)
;;
;; 参数
;; ----
;; str : string
;; 行内文本。
;;
;; 返回值
;; ----
;; list
;; 返回 md-stem 行内元素列表。

(import (scheme base))
(import (liii check))
(import (liii cmark stem inline))

(check-set-mode! 'report-failed)

(check (inline->stem-list "a *b* c") => '("a " (em "b") " c"))

(check-report)
