;; dispatch-pi
;; 根据解析参数分发到对应的 Agent 执行流程。
;;
;; 语法
;; ----
;; (dispatch-pi parsed mode initial-msg rest-msgs)
;;
;; 参数
;; ----
;; parsed : list?
;; 命令行解析后的参数 alist。
;;
;; mode : symbol?
;; 运行模式，如 'interactive、'print 或 'json。
;;
;; initial-msg : any
;; 初始用户消息。
;;
;; rest-msgs : list
;; 后续消息列表。
;;
;; 返回值
;; ----
;; any
;; 匹配流程的执行结果。

(import (liii check) (liii pi code modes))

(check-set-mode! 'report-failed)

;; 1. 验证为过程对象与参数数量
(check-true (procedure? dispatch-pi))
(check (arity dispatch-pi) => '(4 . 4))

(check-report)
