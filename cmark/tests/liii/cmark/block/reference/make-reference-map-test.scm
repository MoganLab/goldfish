;; make-reference-map
;; 创建空的链接引用定义映射表。
;;
;; 语法
;; ----
;; (make-reference-map)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; reference-map
;; 返回空 reference-map 实例。

(import (scheme base))
(import (liii check))
(import (liii cmark block reference))

(check-set-mode! 'report-failed)

(check-true (reference-map? (make-reference-map)))

(check-report)
