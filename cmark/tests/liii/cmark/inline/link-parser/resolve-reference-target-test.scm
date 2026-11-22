;; resolve-reference-target
;; 在引用映射表中查找标签对应的目标与标题。
;;
;; 语法
;; ----
;; (resolve-reference-target refmap label)
;;
;; 参数
;; ----
;; refmap : reference-map 或 #f
;; 引用映射表。
;; label : string
;; 引用标签。
;;
;; 返回值
;; ----
;; pair 或 #f
;; 找到返回 (url . title)，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline link-parser))
(import (liii cmark node))
(import (liii cmark block reference))

(check-set-mode! 'report-failed)

(let ((m (make-reference-map)))
  (reference-map-add-definition! m "x" "https://x.com" "X")
  (check (resolve-reference-target m "x") => '("https://x.com" . "X"))
) ;let

(check-report)
