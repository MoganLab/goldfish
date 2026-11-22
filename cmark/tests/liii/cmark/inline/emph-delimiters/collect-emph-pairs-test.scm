;; collect-emph-pairs
;; 根据扫描到的定界符向量执行 CommonMark 配对算法。
;;
;; 语法
;; ----
;; (collect-emph-pairs delims delims-v n-delims)
;;
;; 参数
;; ----
;; delims : list
;; 定界符列表。
;; delims-v : vector
;; 定界符向量。
;; n-delims : integer
;; 定界符数量。
;;
;; 返回值
;; ----
;; pair
;; 返回配对列表与使用计数。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emph-delimiters))

(check-set-mode! 'report-failed)

(let* ((scanned (scan-emph-delimiters "*a*" 0 #f (lambda args #f) (lambda args #f)))
       (delims scanned)
       (dv (list->vector delims))
       (res (collect-emph-pairs delims dv (vector-length dv)))
      ) ;
  (check-true (pair? (car res)))
) ;let*

(check-report)
