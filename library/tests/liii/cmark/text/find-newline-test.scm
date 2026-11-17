;; find-newline
;; 从 start 起查找下一个换行符索引。
;;
;; 语法
;; ----
;; (find-newline str start)
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
;; integer 或 #f
;; 找到返回换行符下标，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check (find-newline "ab\ncd" 0) => 2)
(check (find-newline "abcd" 0) => #f)

(check-report)
