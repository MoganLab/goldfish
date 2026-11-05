;; pi-event-tool-execution-end
;; 构造 tool_execution_end 事件对象。
;;
;; 语法
;; ----
;; (pi-event-tool-execution-end tool-call-id tool-name result is-error)
;; (pi-event-tool-execution-end tool-call-id tool-name result is-error duration-ms)
;;
;; 参数
;; ----
;; tool-call-id : string?
;; 工具调用标识符。
;;
;; tool-name : string?
;; 工具名称。
;;
;; result : any?
;; 执行结果。
;;
;; is-error : boolean?
;; 是否执行出错。
;;
;; duration-ms : number?
;; 可选，耗时毫秒数。
;;
;; 返回值
;; ----
;; pi-event-tool-execution-end?
;; 工具执行结束事件 record。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(let ((ev (pi-event-tool-execution-end "c1" "bash" "done" #f 100)))
  (check-true (pi-event-tool-execution-end? ev))
  (check (pi-event-tool-execution-end-type ev) => "tool_execution_end")
  (check (pi-event-tool-execution-end-result ev) => "done")
  (check (pi-event-tool-execution-end-is-error ev) => #f)
  (check (pi-event-tool-execution-end-duration-ms ev) => 100)
) ;let

(check-report)
