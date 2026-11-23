;; longest-char-run
;; 计算字符 ch 在 str 中的最长连续游程长度。
;;
;; 语法
;; ----
;; (longest-char-run ch str)
;;
;; 参数
;; ----
;; ch : char
;; 目标字符。
;; str : string
;; 待扫描字符串。
;;
;; 返回值
;; ----
;; integer
;; 最长连续游程长度；不含 ch 时为 0。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown util))

(check-set-mode! 'report-failed)

(check-true (procedure? longest-char-run))

(check (longest-char-run #\` "abc") => 0)
(check (longest-char-run #\` "a`b``c") => 2)
(check (longest-char-run #\` "````x``") => 4)
(check (longest-char-run #\~ "~~~") => 3)
(check (longest-char-run #\` "") => 0)

(check-report)
