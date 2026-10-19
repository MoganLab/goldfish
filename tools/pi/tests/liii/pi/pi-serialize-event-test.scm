(import (liii check) (liii string) (liii pi) (liii json))

(check-set-mode! 'report-failed)

;; pi-serialize-event
;; 将事件或消息 alist 序列化为单行 JSON 字符串。
(let ((json-str (pi-serialize-event '(("type" . "test") ("key" . "value")))))
  (check-true (string-contains? json-str "type"))
) ;let

;; 缺陷8防回归测试：JSON 模式事件文本与各类包含 4 字节 UTF-8 字符及 '<' 的组合
(let* ((ev-user (pi-serialize-event '(("role" . "user") ("content" . "🚀 <"))))
       (parsed-user (string->json ev-user))
      ) ;
  (check-true (string-contains? ev-user "🚀 <"))
  (check (cdr (assoc "content" parsed-user)) => "🚀 <")
) ;let*

(let* ((ev-cjk4 (pi-serialize-event '(("role" . "user") ("content" . "𠀀 <"))))
       (parsed-cjk4 (string->json ev-cjk4))
      ) ;
  (check-true (string-contains? ev-cjk4 "𠀀 <"))
  (check (cdr (assoc "content" parsed-cjk4)) => "𠀀 <")
) ;let*

(let* ((ev-xml (pi-serialize-event '(("role" . "user")
                                     ("content" . "🚀 x <xml> end")))
       ) ;ev-xml
       (parsed-xml (string->json ev-xml))
      ) ;
  (check-true (string-contains? ev-xml "🚀 x <xml> end"))
  (check (cdr (assoc "content" parsed-xml)) => "🚀 x <xml> end")
) ;let*

(let* ((ev-upd
         (pi-serialize-event (pi-event-message-update '((input . 10)
                                                        (output . 5))
                               '(("type" . "text_delta") ("delta" . "🚀 <"))
                             ) ;pi-event-message-update
         ) ;pi-serialize-event
       ) ;ev-upd
       (parsed-upd (string->json ev-upd))
      ) ;
  (check-true (string-contains? ev-upd "🚀 <"))
  (check (cdr (assoc "type" parsed-upd)) => "message_update")
) ;let*

;; 缺陷11防回归测试：toolCall 块在参数为空列表 '() 时序列化为 {"arguments":{}} 而非 {"arguments":[]}
(let* ((tc (pi-tool-call-block "tc_01" "ls" '()))
       (msg (pi-assistant-message (list tc)))
       (json-str (pi-serialize-event msg))
       (parsed (string->json json-str))
       (content (cdr (assoc "content" parsed)))
       (tc-parsed (vector-ref content 0)))
  (check-true (string-contains? json-str "\"arguments\":{}"))
  (check (cdr (assoc "name" tc-parsed)) => "ls")
  (check (cdr (assoc "arguments" tc-parsed)) => '(())))

(check-report)
