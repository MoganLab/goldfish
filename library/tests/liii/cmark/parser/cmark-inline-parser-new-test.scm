
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark inline parser))

(check-set-mode! 'report-failed)

;; cmark-inline-parser-new
;; 创建已初始化的行内解析器。
;;
;; 语法
;; ----
;; (cmark-inline-parser-new)

(let ((parser (cmark-inline-parser-new)))
  (check-true (cmark-inline-parser? parser))
  (check (cmark-inline-parser-content parser) => "")
  (check (cmark-inline-parser-pos parser) => 0)
) ;let

(check-report)
