;; pi-event-tool-execution-start
;; 构造 tool_execution_start 事件对象。
;;
;; 语法
;; ----
;; (pi-event-tool-execution-start tool-call-id tool-name args)
;;
;; 参数
;; ----
;; tool-call-id : string?
;; 工具调用标识符。
;;
;; tool-name : string?
;; 工具名称。
;;
;; args : any?
;; 工具参数。
;;
;; 返回值
;; ----
;; pi-event-tool-execution-start?
;; 工具执行开始事件 record。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(let ((ev (pi-event-tool-execution-start "c1" "bash" '(("cmd" . "ls")))))
  (check-true (pi-event-tool-execution-start? ev))
  (check (pi-event-tool-execution-start-type ev) => "tool_execution_start")
  (check (pi-event-tool-execution-start-tool-call-id ev) => "c1")
  (check (pi-event-tool-execution-start-tool-name ev) => "bash")
  (check (pi-event-tool-execution-start-args ev) => '(("cmd" . "ls")))
) ;let

(check-report)
