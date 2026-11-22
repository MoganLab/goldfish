;; marker-run-len
;; 统计从 pos 起连续相同强调标记字符（* 或 _）的长度。
;;
;; 语法
;; ----
;; (marker-run-len str pos)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; pos : integer
;; 起始位置。
;;
;; 返回值
;; ----
;; integer
;; 返回连续标记长度。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emphasis))

(check-set-mode! 'report-failed)

(check (marker-run-len "***a" 0) => 3)

(check-report)
