;; raw-string-datum?
;; 判断 datum 是否为原始字符串字面量的内部形式。
;;
;; 语法
;; ----
;; (raw-string-datum? datum)
;;
;; 参数
;; ----
;; datum : any?
;; 待检查的 datum。
;;
;; 返回值
;; ------
;; boolean?
;; 返回 #t 表示 datum 是 (*goldfmt-raw-string-<nonce>* 源码 值) 形式，
;; 标记符号来自 (liii goldfmt marker)，用户源码不可能包含。

(import (liii check) (liii goldfmt format datum) (liii goldfmt marker))

(check-set-mode! 'report-failed)

(check (raw-string-datum? (list %raw-string-marker-symbol "#\"x\"" "x")) => #t)
(check (raw-string-datum? (list %raw-string-marker-symbol "#\"x\"")) => #f)
;; 用户写的 (*raw-string* ...) 是普通列表，不是内部标记
(check (raw-string-datum? (list '*raw-string* "#\"x\"" "x")) => #f)

(check-report)
