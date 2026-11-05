;; %make-pi-event-tool-execution-start
;; 构造工具执行开始事件 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-event-tool-execution-start tool-call-id tool-name args)
;;
;; 参数
;; ----
;; tool-call-id : string?
;; 工具调用标识
;;
;; tool-name : string?
;; 工具名称
;;
;; args : alist?
;; 调用参数关联表
;;
;; 返回值
;; ----
;; pi-event-tool-execution-start?
;; 工具执行开始事件 record 实例。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-tool-execution-start "tc-001" "read" '(("path"
                                                                   . "foo.txt")))
      ) ;obj
     ) ;
  (check-true (pi-event-tool-execution-start? obj))
  (check (pi-event-tool-execution-start-tool-call-id obj) => "tc-001")
  (check (pi-event-tool-execution-start-tool-name obj) => "read")
  (check (pi-event-tool-execution-start-args obj) => '(("path" . "foo.txt")))
) ;let

(check-report)
