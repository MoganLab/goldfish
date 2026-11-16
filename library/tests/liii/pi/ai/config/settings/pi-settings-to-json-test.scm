;; pi-settings->json
;; 将 <pi-settings> record 转换为 Scheme-JSON 对象（alist）。
;;
;; 语法
;; ----
;; (pi-settings->json s) -> alist
;;
;; 参数
;; ----
;; s : pi-settings?
;; 全局设置 record。
;;
;; 返回值
;; ----
;; alist
;; 可由 json->string 序列化的 JSON 对象，且可被 json->pi-settings 往返还原。

(import (scheme base)
  (liii check)
  (liii json)
  (liii pi ai config retry)
  (liii pi ai config settings)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? pi-settings->json))

;; 完整字段往返还原
(let* ((orig (pi-settings "deepseek"
               "deepseek-chat"
               "high"
               "http://127.0.0.1:7890"
               (pi-retry-config #f 2 1000 10000 4)
               '("read" "bash")
               "/tmp/sessions"
               #t
               '(("deepseek/deepseek-chat" . "medium"))
               #f
             ) ;pi-settings
       ) ;orig
       (j (pi-settings->json orig))
       (rt (json->pi-settings (string->json (json->string j))))
      ) ;
  (check (cdr (assoc "defaultProvider" j)) => "deepseek")
  (check (cdr (assoc "defaultModel" j)) => "deepseek-chat")
  (check (cdr (assoc "defaultThinkingLevel" j)) => "high")
  (check (cdr (assoc "httpProxy" j)) => "http://127.0.0.1:7890")
  (check (cdr (assoc "defaultTools" j)) => #("read" "bash"))
  (check (cdr (assoc "sessionDir" j)) => "/tmp/sessions")
  (check (cdr (assoc "hideThinkingBlock" j)) => 'true)
  (check (cdr (assoc "enableSkillCommands" j)) => 'false)
  (check-true (pi-settings? rt))
  (check (pi-settings-default-provider rt) => "deepseek")
  (check (pi-settings-default-model rt) => "deepseek-chat")
  (check (pi-settings-default-thinking-level rt) => "high")
  (check (pi-settings-http-proxy rt) => "http://127.0.0.1:7890")
  (check (pi-retry-config-enabled (pi-settings-retry rt)) => #f)
  (check (pi-retry-config-max-retries (pi-settings-retry rt)) => 2)
  (check (pi-retry-config-provider-max-retries (pi-settings-retry rt)) => 4)
  (check (pi-settings-default-tools rt) => '("read" "bash"))
  (check (pi-settings-session-dir rt) => "/tmp/sessions")
  (check (pi-settings-hide-thinking-block rt) => #t)
  (check (pi-settings-model-thinking-level rt "deepseek" "deepseek-chat") => "medium")
  (check (pi-settings-enable-skill-commands rt) => #f)
) ;let*

;; 默认构造省略可选 #f 字段并能往返还原
(let* ((orig (pi-settings))
       (j (pi-settings->json orig))
       (rt (json->pi-settings (string->json (json->string j))))
      ) ;
  (check (assoc "defaultThinkingLevel" j) => #f)
  (check (assoc "httpProxy" j) => #f)
  (check (assoc "defaultTools" j) => #f)
  (check (assoc "sessionDir" j) => #f)
  (check (assoc "modelThinkingLevels" j) => #f)
  (check (pi-settings-default-provider rt) => "google")
  (check (pi-settings-default-model rt) => "gemini-3.8-flash")
  (check (pi-settings-enable-skill-commands rt) => #t)
) ;let*

(check-report)
