;; pi-event-tool-execution-start?
;; 判断对象是否为工具执行开始事件 record 实例。
;;
;; 语法
;; ----
;; (pi-event-tool-execution-start? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于工具执行开始事件 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-tool-execution-start "tc-001" "read" '(("path"
                                                                   . "foo.txt")))
      ) ;obj
     ) ;
  (check-true (pi-event-tool-execution-start? obj))
) ;let
(check-false (pi-event-tool-execution-start? '()))
(check-false (pi-event-tool-execution-start? #f))
(check-false (pi-event-tool-execution-start? "not-a-record"))

(check-report)
