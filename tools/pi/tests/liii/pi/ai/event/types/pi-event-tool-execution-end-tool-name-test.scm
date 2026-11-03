;; pi-event-tool-execution-end-tool-name
;; 获取工具执行结束事件 record 的 tool-name 字段。
;;
;; 语法
;; ----
;; (pi-event-tool-execution-end-tool-name obj)
;;
;; 参数
;; ----
;; obj : any?
;; 工具执行结束事件 record 实例。
;;
;; 返回值
;; ----
;; string?
;; tool-name 字段的值。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-event-tool-execution-end "tc-001" "read" "文件内容" #f 42))
       (obj-b (%make-pi-event-tool-execution-end "tc-002" "bash" "command not found" #t #f)
       ) ;obj-b
      ) ;
  (check (pi-event-tool-execution-end-tool-name obj-a) => "read")
  (check (pi-event-tool-execution-end-tool-name obj-b) => "bash")
) ;let*

(check-report)
