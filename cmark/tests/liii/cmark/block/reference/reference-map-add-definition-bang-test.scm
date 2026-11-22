;; reference-map-add-definition!
;; 向 reference-map 中添加一条链接引用定义（先到优先）。
;;
;; 语法
;; ----
;; (reference-map-add-definition! refmap label url title)
;;
;; 参数
;; ----
;; refmap : reference-map
;; 引用映射表。
;; label : string
;; 引用标签。
;; url : string
;; 目标 URL。
;; title : string
;; 可选标题。
;;
;; 返回值
;; ----
;; unspecified
;; 原地添加定义。

(import (scheme base))
(import (liii check))
(import (liii cmark block reference))

(check-set-mode! 'report-failed)

(let ((m (make-reference-map)))
  (reference-map-add-definition! m "doc" "https://a.b" "Doc")
  (check (reference-map-resolve m "DOC") => '("doc" "https://a.b" . "Doc"))
) ;let

(check-report)
