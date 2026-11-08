;; string%slice
;; 字符串宽容切片（对标 Scala String.slice）。
;;
;; 语法
;; ----
;; ($ s :slice from until)
;;
;; 参数
;; ----
;; from : integer?
;; 起始下标（含），负数按 0。
;;
;; until : integer?
;; 结束下标（不含），超过长度按长度。
;;
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; string?
;; [from, until) 子串，越界/负数 clamp。UTF-8 字符序。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "abcd" :slice 1 3) => "bc")
(check ($ "测试法好" :slice 1 3) => "试法")
(check ($ "abc" :slice -5 2) => "ab")
(check ($ "abc" :slice 2 99) => "c")
(check ($ "abc" :slice 2 1) => "")

(check-report)
