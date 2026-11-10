;; %make-pi-event-tool-execution-end
;; 构造工具执行结束事件 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-event-tool-execution-end tool-call-id tool-name result is-error duration-ms)
;;
;; 参数
;; ----
;; tool-call-id : string?
;; 工具调用标识
;;
;; tool-name : string?
;; 工具名称
;;
;; result : any?
;; 工具执行结果
;;
;; is-error : boolean?
;; 执行是否失败
;;
;; duration-ms : number? 或 #f
;; 执行耗时毫秒数，未知时为 #f
;;
;; 返回值
;; ----
;; pi-event-tool-execution-end?
;; 工具执行结束事件 record 实例。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-tool-execution-end "tc-001" "read" "文件内容" #f 42)))
  (check-true (pi-event-tool-execution-end? obj))
  (check (pi-event-tool-execution-end-tool-call-id obj) => "tc-001")
  (check (pi-event-tool-execution-end-tool-name obj) => "read")
  (check (pi-event-tool-execution-end-result obj) => "文件内容")
  (check (pi-event-tool-execution-end-is-error obj) => #f)
  (check (pi-event-tool-execution-end-duration-ms obj) => 42)
) ;let

(check-report)
