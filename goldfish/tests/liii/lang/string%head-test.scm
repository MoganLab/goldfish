;; string%head / string%last（对标 Scala String.head/last）
;; 字符串首末字符，UTF-8 语义。
;;
;; 语法
;; ----
;; ($ s :head)   ($ s :last)   ($ s :head-option)   ($ s :last-option)

;; 参数
;; ----
;; s : string?
;; 源字符串，不可为空。
;;
;; 返回值
;; ------
;; :head/:last —— 首末字符 char?，空串抛 value-error；
;; :head-option/:last-option —— (option c)，空串 (none)。
;;
(import (liii check) (liii lang) (liii option) (liii error))

(check-set-mode! 'report-failed)

(check ($ "测试法" :head) => #\测)
(check ($ "测试法" :last) => #\法)
(check ($ "a" :head) => #\a)
(check-catch 'value-error ($ "" :head))
(check ($ "测试" :head-option) => (option #\测))
(check ($ "" :last-option) => (none))

(check-report)
