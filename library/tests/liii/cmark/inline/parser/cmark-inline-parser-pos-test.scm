;; cmark-inline-parser-pos
;; 获取内联解析器的当前扫描位置。
;;
;; 语法
;; ----
;; (cmark-inline-parser-pos parser)
;;
;; 参数
;; ----
;; parser : cmark-inline-parser
;; 解析器实例。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 返回 pos 字段。

(import (scheme base))
(import (liii check))
(import (liii cmark inline parser))

(check-set-mode! 'report-failed)

(let ((p (cmark-inline-parser-new)))
  (check (cmark-inline-parser-pos p) => 0)
) ;let

(check-report)
