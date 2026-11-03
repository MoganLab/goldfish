;; pi-event-tool-execution-update-partial-result
;; 获取工具执行更新事件 record 的 partial-result 字段。
;;
;; 语法
;; ----
;; (pi-event-tool-execution-update-partial-result obj)
;;
;; 参数
;; ----
;; obj : any?
;; 工具执行更新事件 record 实例。
;;
;; 返回值
;; ----
;; string? 或 #f
;; partial-result 字段的值。

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
  (check (pi-event-tool-execution-update-partial-result obj-a) => "读取中")
  (check (pi-event-tool-execution-update-partial-result obj-b) => "执行中")
) ;let*

(check-report)
