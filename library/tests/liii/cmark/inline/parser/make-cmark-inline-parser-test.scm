;; make-cmark-inline-parser
;; 创建未初始化的内联解析器记录。
;;
;; 语法
;; ----
;; (make-cmark-inline-parser)
;;
;;
;; 参数
;; ----
;; 无参数。
;; 返回值
;; ----
;; cmark-inline-parser
;; 返回字段初始值均为 #f 的记录实例。
;;
;; 描述
;; ----
;; 该构造器主要用于底层初始化和字段访问测试。

(import (liii check) (liii cmark inline parser))

(check-set-mode! 'report-failed)

(let ((parser (make-cmark-inline-parser)))
  (check-true (cmark-inline-parser? parser))
  (check (cmark-inline-parser-content parser) => #f)
  (check (cmark-inline-parser-pos parser) => #f)
  (check (cmark-inline-parser-node parser) => #f)
  (cmark-inline-parser-set-content! parser "inline")
  (cmark-inline-parser-set-pos! parser 2)
  (cmark-inline-parser-set-node! parser 'sentinel)
  (check (cmark-inline-parser-content parser) => "inline")
  (check (cmark-inline-parser-pos parser) => 2)
  (check (cmark-inline-parser-node parser) => 'sentinel)
) ;let

(check-report)
