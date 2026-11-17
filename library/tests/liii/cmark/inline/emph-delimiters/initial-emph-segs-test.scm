;; initial-emph-segs
;; 构造强调区域 [lo, hi) 的初始片段列表。
;;
;; 语法
;; ----
;; (initial-emph-segs str delims-v n-delims lo hi)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; delims-v : vector
;; 定界符向量。
;; n-delims : integer
;; 定界符数量。
;; lo : integer
;; 起始下标。
;; hi : integer
;; 结束下标。
;;
;; 返回值
;; ----
;; list
;; 返回初始片段列表。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emph-delimiters))

(check-set-mode! 'report-failed)

(let ((segs (initial-emph-segs "abc" #() 0 0 3)))
  (check (length segs) => 1)
) ;let

(check-report)
