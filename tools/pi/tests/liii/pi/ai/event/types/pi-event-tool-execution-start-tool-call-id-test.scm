;; pi-event-tool-execution-start-tool-call-id
;; 获取工具执行开始事件 record 的 tool-call-id 字段。
;;
;; 语法
;; ----
;; (pi-event-tool-execution-start-tool-call-id obj)
;;
;; 参数
;; ----
;; obj : any?
;; 工具执行开始事件 record 实例。
;;
;; 返回值
;; ----
;; string?
;; tool-call-id 字段的值。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-event-tool-execution-start "tc-001" "read" '(("path"
                                                                      . "foo.txt")))
       ) ;obj-a
       (obj-b (%make-pi-event-tool-execution-start "tc-002" "bash" '(("command"
                                                                      . "ls")))
       ) ;obj-b
      ) ;
  (check (pi-event-tool-execution-start-tool-call-id obj-a) => "tc-001")
  (check (pi-event-tool-execution-start-tool-call-id obj-b) => "tc-002")
) ;let*

(check-report)
