;; split-segs-at
;; 在指定字符位置 pos 处切分片段列表。
;;
;; 语法
;; ----
;; (split-segs-at segs pos)
;;
;; 参数
;; ----
;; segs : list
;; 片段列表。
;; pos : integer
;; 切分位置。
;;
;; 返回值
;; ----
;; pair
;; 返回 (left-segs . right-segs)。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emph-delimiters))

(check-set-mode! 'report-failed)

(let ((res (split-segs-at (list (cons 'text (cons 0 4))) 2)))
  (check-true (pair? res))
) ;let

(check-report)
