;; pi-thinking-block?
;; 判断对象是否为思考内容块 record 实例。
;;
;; 语法
;; ----
;; (pi-thinking-block? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于思考内容块 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-thinking-block "先分析需求" "sig-001")))
  (check-true (pi-thinking-block? obj))
) ;let
(check-false (pi-thinking-block? '()))
(check-false (pi-thinking-block? #f))
(check-false (pi-thinking-block? "not-a-record"))

(check-report)
