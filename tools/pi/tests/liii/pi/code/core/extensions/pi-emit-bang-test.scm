(import (liii check) (liii pi code core extensions))

(check-set-mode! 'report-failed)

;; pi-emit!
;; 依次调用指定事件的扩展监听器，并汇总继续、阻断或输入覆盖结果。
;;
;; 语法
;; ----
;; (pi-emit! target event-type payload) -> alist
;;
;; 参数
;; ----
;; target : extension-registry | extension-api | extension-context
;; 事件所属的扩展对象。
;; event-type : symbol | string
;; 事件名称。
;; payload : any
;; 传给监听器的数据；输入覆盖事件可更新其中的 input/params 字段。
;;
;; 返回值
;; ----
;; alist
;; 通常包含 action 和 payload；监听器可请求 block、terminate 或覆盖输入。
;; 单个监听器抛错会被隔离，后续监听器仍继续执行。
;;
;; 示例
;; ----
;; (pi-emit! registry 'tool_execution_start '((input . "args")))

(let ((api (make-extension-api)))
  (check (pi-emit! api 'missing '((input . "old")))
    =>
    '((action . continue) (payload (input . "old")))
  ) ;check
) ;let

(let ((api (make-extension-api)))
  (pi-on! api 'override-input (lambda (payload ctx) (cons 'override "new")))
  (check (pi-emit! api "override-input" '((input . "old") (keep . 1)))
    =>
    '((action . continue) (payload (input . "new") (keep . 1)))
  ) ;check
) ;let

(check-report)
