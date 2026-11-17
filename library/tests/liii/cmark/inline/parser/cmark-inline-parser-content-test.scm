;; cmark-inline-parser-content
;; 获取内联解析器的输入字符串。
;;
;; 语法
;; ----
;; (cmark-inline-parser-content parser)
;;
;; 参数
;; ----
;; parser : cmark-inline-parser
;; 解析器实例。
;;
;; 返回值
;; ----
;; string 或 #f
;; 返回 content 字段。

(import (scheme base))
(import (liii check))
(import (liii cmark inline parser))

(check-set-mode! 'report-failed)

(let ((p (cmark-inline-parser-new)))
  (check (cmark-inline-parser-content p) => "")
) ;let

(check-report)
