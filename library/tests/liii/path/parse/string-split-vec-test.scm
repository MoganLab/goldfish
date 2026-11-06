;; string-split-vec
;; 将字符串按分隔符切分为向量（保留空段）。
;;
;; 语法
;; ----
;; (string-split-vec str sep)
;;
;; 参数
;; ----
;; str : string?
;; 待切分的字符串。
;; sep : char?
;; 分隔符。
;;
;; 返回值
;; ----
;; vector
;; 切分结果向量，保留空段，Windows/posix 通用。

(import (liii check) (liii path parse))

(check-set-mode! 'report-failed)

(check (string-split-vec "a/b/c" #\/) => #("a" "b" "c"))
(check (string-split-vec "a//b" #\/) => #("a" "" "b"))
(check (string-split-vec "" #\/) => #(""))
(check (string-split-vec "/a" #\/) => #("" "a"))

(check-report)
