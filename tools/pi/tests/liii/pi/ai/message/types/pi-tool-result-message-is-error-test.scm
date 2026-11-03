;; pi-tool-result-message-is-error
;; 获取工具执行结果消息 record 的 is-error 字段。
;;
;; 语法
;; ----
;; (pi-tool-result-message-is-error obj)
;;
;; 参数
;; ----
;; obj : any?
;; 工具执行结果消息 record 实例。
;;
;; 返回值
;; ----
;; boolean?
;; is-error 字段的值。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-tool-result-message "call-001" "read" "文件内容" #f 5000))
       (obj-b (%make-pi-tool-result-message "call-002" "bash" "command not found" #t 6000)
       ) ;obj-b
      ) ;
  (check (pi-tool-result-message-is-error obj-a) => #f)
  (check (pi-tool-result-message-is-error obj-b) => #t)
) ;let*

(check-report)
