;; pi-thinking-level?
;; 判断给定对象是否属于合法的思考级别字符串。
;;
;; 语法
;; ----
;; (pi-thinking-level? level)
;;
;; 参数
;; ----
;; level : any?
;; 待检验的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若在级别全集内返回 #t，否则返回 #f。

(import (liii check) (liii pi ai thinking))

(check-set-mode! 'report-failed)

(check-true (pi-thinking-level? "off"))
(check-true (pi-thinking-level? "max"))
(check-false (pi-thinking-level? "ludicrous"))
(check-false (pi-thinking-level? #f))
(check-false (pi-thinking-level? 42))

(check-report)
