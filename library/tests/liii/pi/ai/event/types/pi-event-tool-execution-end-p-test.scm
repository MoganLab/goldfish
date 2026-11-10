;; pi-event-tool-execution-end?
;; 判断对象是否为工具执行结束事件 record 实例。
;;
;; 语法
;; ----
;; (pi-event-tool-execution-end? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于工具执行结束事件 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-tool-execution-end "tc-001" "read" "文件内容" #f 42)))
  (check-true (pi-event-tool-execution-end? obj))
) ;let
(check-false (pi-event-tool-execution-end? '()))
(check-false (pi-event-tool-execution-end? #f))
(check-false (pi-event-tool-execution-end? "not-a-record"))

(check-report)
