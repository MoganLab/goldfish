;; pi-boundary-recover-messages
;; 回滚会话尾部不完整的 assistant 消息，保留已完成历史。
;;
;; 语法
;; ----
;; (pi-boundary-recover-messages messages)
;;
;; 参数
;; ----
;; messages : list
;; 原始会话消息列表，支持消息记录与 alist。
;;
;; 返回值
;; ----
;; list
;; 尾部回滚到用户消息、已完成 tool_result 或完整 assistant 的新列表。

(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi code all-in-one boundary)
) ;import

(check-set-mode! 'report-failed)

(define USER (pi-user-message "run the tool"))

(define TOOL-CALL
  (pi-assistant-message (list (pi-tool-call-block "c1" "read" '())) "toolUse")
) ;define

(define TOOL-RESULT
  (pi-tool-result-message "c1" "read" (list (pi-text-block "ok")))
) ;define

(define BROKEN
  (pi-assistant-message (list (pi-text-block "half answer")) "error" "HTTP 529")
) ;define

;; 1. 半截 assistant 回滚到上一轮真实执行完成的 tool_result
(let ((recovered (pi-boundary-recover-messages (list USER TOOL-CALL TOOL-RESULT BROKEN))
      ) ;recovered
     ) ;
  (check (length recovered) => 3)
  (check recovered => (list USER TOOL-CALL TOOL-RESULT))
  (check (car (reverse recovered)) => TOOL-RESULT)
) ;let

;; 2. 连续残留错误消息逐条弹出，用户指令保持不变
(let* ((aborted (pi-assistant-message '() "aborted"))
       (recovered (pi-boundary-recover-messages (list USER BROKEN aborted)))
      ) ;
  (check recovered => (list USER))
) ;let*

;; 3. 完整历史与 tool_result 边界原样保留，包括工具本身的失败结果
(let ((messages (list USER TOOL-CALL TOOL-RESULT)))
  (check (pi-boundary-recover-messages messages) => messages)
) ;let
(let* ((failed-result (pi-tool-result-message "c1" "read" "not found" #t))
       (messages (list USER TOOL-CALL failed-result))
      ) ;
  (check (pi-boundary-recover-messages messages) => messages)
) ;let*
(let ((messages
        (list USER (pi-assistant-message (list (pi-text-block "done")) "stop"))
      ) ;messages
     ) ;
  (check (pi-boundary-recover-messages messages) => messages)
) ;let

;; 4. alist 会话采用同一规则；非列表输入安全降级为空上下文
(check
  (pi-boundary-recover-messages '(((role . "user") (content . "go"))
                                  ((role . "assistant")
                                   (content . "partial")
                                   (stopReason . "aborted")))
  ) ;pi-boundary-recover-messages
  =>
  '(((role . "user") (content . "go")))
) ;check
(check (pi-boundary-recover-messages '()) => '())
(check (pi-boundary-recover-messages #f) => '())

(check-report)
