
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark inline-parser))

(check-set-mode! 'report-failed)

;; cmark-inline-parser-new
;; 创建已初始化的内联解析器。
;;
;; 语法
;; ----
;; (cmark-inline-parser-new)
;;
;; 返回值
;; ----
;; cmark-inline-parser
;; 返回 content 为空串、pos 为 0、node 为 #f 的解析器。

(let ((parser (cmark-inline-parser-new)))
  (check-true (cmark-inline-parser? parser))
  (check (cmark-inline-parser-content parser) => "")
  (check (cmark-inline-parser-pos parser) => 0)
  (check (cmark-inline-parser-node parser) => #f)
) ;let

(check-report)
