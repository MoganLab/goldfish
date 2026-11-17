;; scan-emph-delimiters
;; 扫描输入字符串中的强调定界符以及内嵌链接/代码分段。
;;
;; 语法
;; ----
;; (scan-emph-delimiters str start refmap parse-link-fn parse-image-fn)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; start : integer
;; 起始位置。
;; refmap : reference-map 或 #f
;; 引用映射。
;; parse-link-fn : procedure
;; 链接回调。
;; parse-image-fn : procedure
;; 图片回调。
;;
;; 返回值
;; ----
;; vector
;; 返回定界符扫描结果。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emph-delimiters))

(check-set-mode! 'report-failed)

(let ((res (scan-emph-delimiters "*a*" 0 #f (lambda args #f) (lambda args #f))))
  (check-true (pair? res))
) ;let

(check-report)
