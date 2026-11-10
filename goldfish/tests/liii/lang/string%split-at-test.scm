;; string%split-at
;; 在下标 n 处将字符串二分（对标 Scala String.splitAt）。
;;
;; 语法
;; ----
;; ($ s :split-at n)
;;
;; 参数
;; ----
;; n : integer?
;; 切分位置。
;;
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; list?
;; ("take n" "drop n") 二元组，UTF-8 字符序。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "abcde" :split-at 2) => '("ab" "cde"))
(check ($ "测试法" :split-at 1) => '("测" "试法"))
(check ($ "abc" :split-at 10) => '("abc" ""))
(check ($ "abc" :split-at -1) => '("" "abc"))
(check ($ "" :split-at 2) => '("" ""))

(check-report)
