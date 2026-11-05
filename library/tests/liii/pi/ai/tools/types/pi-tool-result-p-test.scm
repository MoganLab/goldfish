;; pi-tool-result?
;; 判断对象是否为工具执行结果 record 实例。
;;
;; 语法
;; ----
;; (pi-tool-result? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于工具执行结果 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai tools types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-tool-result "执行成功" #f)))
  (check-true (pi-tool-result? obj))
) ;let
(check-false (pi-tool-result? '()))
(check-false (pi-tool-result? #f))
(check-false (pi-tool-result? "not-a-record"))

(check-report)
