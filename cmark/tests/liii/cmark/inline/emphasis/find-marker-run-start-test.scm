;; find-marker-run-start
;; 回退查找 pos 所在连续强调标记串的起始位置。
;;
;; 语法
;; ----
;; (find-marker-run-start str pos)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; pos : integer
;; 当前位置。
;;
;; 返回值
;; ----
;; integer
;; 返回连续标记串的起始索引。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emphasis))

(check-set-mode! 'report-failed)

(check (find-marker-run-start "a***b" 3) => 1)

(check-report)
