;; pi-serialize-event
;; 将事件或消息序列化为单行 JSON 字符串。
;;
;; 语法
;; ----
;; (pi-serialize-event alist)
;;
;; 参数
;; ----
;; alist : any?
;; 事件、消息或 content block，可包含嵌套 alist 和 vector。
;;
;; 返回值
;; ----
;; string?
;; 规范化后的单行 JSON 字符串。
;;
;; 说明
;; ----
;; errorMessage=#f 被过滤，空 arguments/input 规范化为 JSON 对象。
;; 对象 entry 顺序和 Unicode 文本保持不变。

(import (liii check)
  (liii string)
  (liii pi ai event)
  (liii pi ai message)
  (liii json)
) ;import

(check-set-mode! 'report-failed)

(let ((json-str (pi-serialize-event '(("type" . "test") ("key" . "value")))))
  (check-true (string-contains? json-str "type"))
) ;let

;; 缺陷8防回归测试：JSON 模式事件文本与各类包含 4 字节 UTF-8 字符及 '<' 的组合
(let* ((ev-user (pi-serialize-event '(("role" . "user") ("content" . "🚀 <"))))
       (parsed-user (string->json ev-user))
      ) ;
  (check-true (string? ev-user))
  (check-true (pair? parsed-user))
  (check (cdr (assoc "content" parsed-user)) => "🚀 <")
) ;let*

(let* ((ev-text (pi-serialize-event '(("type" . "text")
                                      ("text" . "💡 <test> 🌟")))
       ) ;ev-text
       (parsed-text (string->json ev-text))
      ) ;
  (check-true (string? ev-text))
  (check-true (pair? parsed-text))
  (check (cdr (assoc "text" parsed-text)) => "💡 <test> 🌟")
) ;let*

(let* ((ev-think (pi-serialize-event '(("type" . "thinking")
                                       ("thinking" . "🤔 <thought> 🎯"))
                 ) ;pi-serialize-event
       ) ;ev-think
       (parsed-think (string->json ev-think))
      ) ;
  (check-true (string? ev-think))
  (check-true (pair? parsed-think))
  (check (cdr (assoc "thinking" parsed-think)) => "🤔 <thought> 🎯")
) ;let*

(let* ((ev-tool
         (pi-serialize-event '(("type" . "toolCall")
                               ("name" . "bash")
                               ("arguments" ("cmd" . "echo '🎉 <hello>'")))
         ) ;pi-serialize-event
       ) ;ev-tool
       (parsed-tool (string->json ev-tool))
      ) ;
  (check-true (string? ev-tool))
  (check-true (pair? parsed-tool))
  (let ((args (cdr (assoc "arguments" parsed-tool))))
    (check (cdr (assoc "cmd" args)) => "echo '🎉 <hello>'")
  ) ;let
) ;let*

(let* ((ev-msg (pi-serialize-event '(("role" . "assistant")
                                     ("content"
                                      . #((("type" . "text")
                                           ("text" . "✨ <response> 🎈"))
                                        ) ;#
                                     ))
               ) ;pi-serialize-event
       ) ;ev-msg
       (parsed-msg (string->json ev-msg))
      ) ;
  (check-true (string? ev-msg))
  (check-true (pair? parsed-msg))
  (let ((content (cdr (assoc "content" parsed-msg))))
    (check (vector-length content) => 1)
    (check (cdr (assoc "text" (vector-ref content 0))) => "✨ <response> 🎈")
  ) ;let
) ;let*

(check-report)
