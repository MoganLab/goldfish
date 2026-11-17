;; skip-ascii-whitespace
;; 从 start 起跳过 ASCII 空白字符，返回首个非空白位置。
;;
;; 语法
;; ----
;; (skip-ascii-whitespace str start)
;;
;; 参数
;; ----
;; str : string
;; 目标字符串。
;; start : integer
;; 起始索引。
;;
;; 返回值
;; ----
;; integer
;; 返回首个非空白位置或字符串长度。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check (skip-ascii-whitespace "  \tabc" 0) => 3)
(check (skip-ascii-whitespace "   " 0) => 3)

(check-report)
