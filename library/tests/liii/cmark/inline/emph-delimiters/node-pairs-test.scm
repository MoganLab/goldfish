;; node-pairs
;; 获取强调森林节点关联的配对列表。
;;
;; 语法
;; ----
;; (node-pairs n)
;;
;; 参数
;; ----
;; n : vector
;; 森林节点。
;;
;; 返回值
;; ----
;; any
;; 返回节点对应字段。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emph-delimiters))

(check-set-mode! 'report-failed)

(let* ((scanned (scan-emph-delimiters "*a*" 0 #f (lambda args #f) (lambda args #f)))
       (delims scanned)
       (dv (list->vector delims))
       (paired (collect-emph-pairs delims dv (vector-length dv)))
       (roots (build-emph-forest paired dv))
      ) ;
  (check (length (node-pairs (car roots))) => 1)
) ;let*

(check-report)
