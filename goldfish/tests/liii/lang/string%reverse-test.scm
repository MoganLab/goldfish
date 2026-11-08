;; string%reverse
;; 反转字符串（对标 Scala String.reverse）。
;;
;; 语法
;; ----
;; ($ s :reverse)
;;
;; 参数
;; ----
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; string?
;; 字符顺序完全颠倒的新字符串，UTF-8 多字节字符整体颠倒。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "ab测" :reverse) => "测ba")
(check ($ "abc" :reverse) => "cba")
(check ($ "" :reverse) => "")
(check ($ "🎉a" :reverse) => "a🎉")

(check-report)
