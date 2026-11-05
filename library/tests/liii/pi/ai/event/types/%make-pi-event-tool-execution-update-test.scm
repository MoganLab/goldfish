;; %make-pi-event-tool-execution-update
;; 构造工具执行更新事件 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-event-tool-execution-update tool-call-id tool-name args partial-result)
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
;; partial-result : string? 或 #f
;; 已产出的部分结果文本
;;
;; 返回值
;; ----
;; pi-event-tool-execution-update?
;; 工具执行更新事件 record 实例。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-tool-execution-update "tc-001"
             "read"
             '(("path" . "foo.txt"))
             "读取中"
           ) ;%make-pi-event-tool-execution-update
      ) ;obj
     ) ;
  (check-true (pi-event-tool-execution-update? obj))
  (check (pi-event-tool-execution-update-tool-call-id obj) => "tc-001")
  (check (pi-event-tool-execution-update-tool-name obj) => "read")
  (check (pi-event-tool-execution-update-args obj) => '(("path" . "foo.txt")))
  (check (pi-event-tool-execution-update-partial-result obj) => "读取中")
) ;let

(check-report)
