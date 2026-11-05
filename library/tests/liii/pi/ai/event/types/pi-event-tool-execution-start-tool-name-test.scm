;; pi-event-tool-execution-start-tool-name
;; 获取工具执行开始事件 record 的 tool-name 字段。
;;
;; 语法
;; ----
;; (pi-event-tool-execution-start-tool-name obj)
;;
;; 参数
;; ----
;; obj : any?
;; 工具执行开始事件 record 实例。
;;
;; 返回值
;; ----
;; string?
;; tool-name 字段的值。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-event-tool-execution-start "tc-001" "read" '(("path"
                                                                      . "foo.txt")))
       ) ;obj-a
       (obj-b (%make-pi-event-tool-execution-start "tc-002" "bash" '(("command"
                                                                      . "ls")))
       ) ;obj-b
      ) ;
  (check (pi-event-tool-execution-start-tool-name obj-a) => "read")
  (check (pi-event-tool-execution-start-tool-name obj-b) => "bash")
) ;let*

(check-report)
