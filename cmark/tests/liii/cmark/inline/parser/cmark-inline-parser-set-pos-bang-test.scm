;; cmark-inline-parser-set-pos!
;; 设置内联解析器的当前扫描位置。
;;
;; 语法
;; ----
;; (cmark-inline-parser-set-pos! parser pos)
;;
;; 参数
;; ----
;; parser : cmark-inline-parser
;; 解析器实例。
;; pos : integer
;; 新位置。
;;
;; 返回值
;; ----
;; unspecified
;; 原地更新 pos。

(import (scheme base))
(import (liii check))
(import (liii cmark inline parser))

(check-set-mode! 'report-failed)

(let ((p (cmark-inline-parser-new)))
  (cmark-inline-parser-set-pos! p 5)
  (check (cmark-inline-parser-pos p) => 5)
) ;let

(check-report)
