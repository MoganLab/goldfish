(import (liii check) (liii string) (liii pi ai event) (liii pi ai message))

(check-set-mode! 'report-failed)

;; pi-event-message-update
;; 构造 message_update 流式增量事件对象。
(let* ((usage '(("input" . 10) ("output" . 5) ("total" . 15)))
       (delta '(("type" . "text_delta") ("delta" . "hello")))
       (ev (pi-event-message-update usage delta))
      ) ;
  (check-true (pi-event-message-update? ev))
  (check (pi-event-message-update-type ev) => "message_update")
  (check (pi-event-message-update-usage ev) => usage)
  (check (pi-event-message-update-assistant-message-event ev) => delta)
  ;; 兼容 alist 访问
  (check (cdr (assoc "type" ev)) => "message_update")
  (check (cdr (assoc "usage" ev)) => usage)
  (check (cdr (assoc "assistantMessageEvent" ev)) => delta)
) ;let*

;; text_delta 的字段与序列化
(let* ((usage '(("input" . 10) ("output" . 5) ("total" . 15)))
       (delta-ev '(("type" . "text_delta")
                   ("contentIndex" . 0)
                   ("delta" . "Hello ")))
       (upd (pi-event-message-update usage delta-ev))
      ) ;
  (check (cdr (assoc "type" upd)) => "message_update")
  (check
    (cdr (assoc "input" (cdr (assoc "usage" upd))))
    =>
    10
  ) ;check
  (let ((a-ev (cdr (assoc "assistantMessageEvent" upd))))
    (check (cdr (assoc "type" a-ev)) => "text_delta")
    (check (cdr (assoc "contentIndex" a-ev)) => 0)
    (check (cdr (assoc "delta" a-ev)) => "Hello ")
  ) ;let
  (let ((serialized (pi-serialize-event upd)))
    (check-true (string? serialized))
    (check-true (string-contains serialized "\"type\":\"message_update\""))
    (check-true (string-contains serialized "\"delta\":\"Hello \""))
  ) ;let
) ;let*

;; toolcall_start 保留 id 与 toolName
(let* ((usage '(("input" . 10) ("output" . 5) ("total" . 15)))
       (tc-ev '(("type" . "toolcall_start")
                ("contentIndex" . 1)
                ("id" . "call_1")
                ("toolName" . "bash"))
       ) ;tc-ev
       (upd (pi-event-message-update usage tc-ev))
      ) ;
  (check (cdr (assoc "type" upd)) => "message_update")
  (let ((a-ev (cdr (assoc "assistantMessageEvent" upd))))
    (check (cdr (assoc "type" a-ev)) => "toolcall_start")
    (check (cdr (assoc "id" a-ev)) => "call_1")
    (check (cdr (assoc "toolName" a-ev)) => "bash")
  ) ;let
) ;let*

(check-report)
