;; cmark-inline-parser?
;; 判断对象是否为 cmark-inline-parser 实例。
;;
;; 语法
;; ----
;; (cmark-inline-parser? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 是实例返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline parser))

(check-set-mode! 'report-failed)

(check-true (cmark-inline-parser? (cmark-inline-parser-new)))
(check-false (cmark-inline-parser? "x"))

(check-report)
