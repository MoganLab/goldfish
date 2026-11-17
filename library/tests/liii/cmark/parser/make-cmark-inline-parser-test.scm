(import (liii check) (liii cmark inline parser))

(check-set-mode! 'report-failed)

;; make-cmark-inline-parser
;; 创建 cmark-parser 内部使用的行内解析器记录。
;;
;; 语法
;; ----
;; (make-cmark-inline-parser)
;;
;; 返回值
;; ----
;; cmark-inline-parser
;; 返回 content 和 pos 初始值均为 #f 的记录实例。

(let ((parser (make-cmark-inline-parser)))
  (check-true (cmark-inline-parser? parser))
  (check (cmark-inline-parser-content parser) => #f)
  (check (cmark-inline-parser-pos parser) => #f)
  (cmark-inline-parser-set-content! parser "inline")
  (cmark-inline-parser-set-pos! parser 3)
  (check (cmark-inline-parser-content parser) => "inline")
  (check (cmark-inline-parser-pos parser) => 3)
) ;let

(check-report)
