;; reference-map-resolve
;; 按规范化标签在 reference-map 中解析引用定义。
;;
;; 语法
;; ----
;; (reference-map-resolve refmap label)
;;
;; 参数
;; ----
;; refmap : reference-map
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
(import (liii cmark block reference))

(check-set-mode! 'report-failed)

(let ((m (make-reference-map)))
  (reference-map-add-definition! m "foo bar" "https://x.y" "")
  (check (reference-map-resolve m "  FOO\nbar ")
    =>
    '("foo bar" "https://x.y" . "")
  ) ;check
  (check (reference-map-resolve m "missing") => #f)
) ;let

(check-report)
