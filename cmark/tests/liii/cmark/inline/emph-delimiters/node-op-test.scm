;; node-op
;; 获取强调森林节点的左定界符索引。
;;
;; 语法
;; ----
;; (node-op n)
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
  (check (node-op (car roots)) => 0)
) ;let*

(check-report)
