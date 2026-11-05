;; pi-event-tool-execution-end-result
;; 获取工具执行结束事件 record 的 result 字段。
;;
;; 语法
;; ----
;; (pi-event-tool-execution-end-result obj)
;;
;; 参数
;; ----
;; obj : any?
;; 工具执行结束事件 record 实例。
;;
;; 返回值
;; ----
;; any?
;; result 字段的值。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-event-tool-execution-end "tc-001" "read" "文件内容" #f 42))
       (obj-b (%make-pi-event-tool-execution-end "tc-002" "bash" "command not found" #t #f)
       ) ;obj-b
      ) ;
  (check (pi-event-tool-execution-end-result obj-a) => "文件内容")
  (check (pi-event-tool-execution-end-result obj-b) => "command not found")
) ;let*

(check-report)
