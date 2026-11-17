;; cmark-inline-parser-set-content!
;; 设置内联解析器的输入字符串。
;;
;; 语法
;; ----
;; (cmark-inline-parser-set-content! parser content)
;;
;; 参数
;; ----
;; parser : cmark-inline-parser
;; 解析器实例。
;; content : string
;; 新输入文本。
;;
;; 返回值
;; ----
;; unspecified
;; 原地更新 content。

(import (scheme base))
(import (liii check))
(import (liii cmark inline parser))

(check-set-mode! 'report-failed)

(let ((p (cmark-inline-parser-new)))
  (cmark-inline-parser-set-content! p "abc")
  (check (cmark-inline-parser-content p) => "abc")
) ;let

(check-report)
