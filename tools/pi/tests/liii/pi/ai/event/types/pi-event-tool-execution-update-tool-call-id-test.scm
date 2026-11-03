;; pi-event-tool-execution-update-tool-call-id
;; 获取工具执行更新事件 record 的 tool-call-id 字段。
;;
;; 语法
;; ----
;; (pi-event-tool-execution-update-tool-call-id obj)
;;
;; 参数
;; ----
;; obj : any?
;; 工具执行更新事件 record 实例。
;;
;; 返回值
;; ----
;; string?
;; tool-call-id 字段的值。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-event-tool-execution-update "tc-001"
                "read"
                '(("path" . "foo.txt"))
                "读取中"
              ) ;%make-pi-event-tool-execution-update
       ) ;obj-a
       (obj-b (%make-pi-event-tool-execution-update "tc-002"
                "bash"
                '(("command" . "ls"))
                "执行中"
              ) ;%make-pi-event-tool-execution-update
       ) ;obj-b
      ) ;
  (check (pi-event-tool-execution-update-tool-call-id obj-a) => "tc-001")
  (check (pi-event-tool-execution-update-tool-call-id obj-b) => "tc-002")
) ;let*

(check-report)
