;; json->pi-settings
;; 将 JSON 对象（settings.json）转换为 <pi-settings> record。
;;
;; 语法
;; ----
;; (json->pi-settings obj)
;;
;; 参数
;; ----
;; obj : any?
;; string->json 解析出的 alist，或已是 <pi-settings> 的 record。
;;
;; 返回值
;; ----
;; pi-settings?
;; 解析后的全局设置 record；缺失字段使用安全默认值。

(import (liii check) (liii json) (liii pi ai config record))

(check-set-mode! 'report-failed)

(check-true (procedure? json->pi-settings))

;; 1. 完整 JSON 解析
(let* ((j (string->json "{\"defaultProvider\":\"deepseek\",\"defaultModel\":\"deepseek-chat\",\"defaultThinkingLevel\":\"high\",\"httpProxy\":\"http://127.0.0.1:7890\",\"retry\":{\"enabled\":false,\"maxRetries\":2},\"defaultTools\":[\"read\",\"bash\"],\"sessionDir\":\"/tmp/sessions\",\"hideThinkingBlock\":true,\"modelThinkingLevels\":{\"deepseek/deepseek-chat\":\"medium\"},\"enableSkillCommands\":false}"))
       (s (json->pi-settings j))
      ) ;
  (check-true (pi-settings? s))
  (check (pi-settings-default-provider s) => "deepseek")
  (check (pi-settings-default-model s) => "deepseek-chat")
  (check (pi-settings-default-thinking-level s) => "high")
  (check (pi-settings-http-proxy s) => "http://127.0.0.1:7890")
  (check-true (pi-retry-config? (pi-settings-retry s)))
  (check (pi-retry-config-enabled (pi-settings-retry s)) => #f)
  (check (pi-retry-config-max-retries (pi-settings-retry s)) => 2)
  (check (pi-settings-default-tools s) => '("read" "bash"))
  (check (pi-settings-session-dir s) => "/tmp/sessions")
  (check (pi-settings-hide-thinking-block s) => #t)
  (check (pi-settings-model-thinking-level s "deepseek" "deepseek-chat") => "medium")
  (check (pi-settings-enable-skill-commands s) => #f)
) ;let*

;; 2. 空对象与空白 httpProxy：使用默认值与安全过滤
(let ((s (json->pi-settings (string->json "{\"httpProxy\":\"   \"}"))))
  (check-true (pi-settings? s))
  (check (pi-settings-default-provider s) => "google")
  (check (pi-settings-default-model s) => "gemini-3.8-flash")
  (check (pi-settings-default-thinking-level s) => #f)
  (check (pi-settings-http-proxy s) => #f)
  (check-true (pi-retry-config? (pi-settings-retry s)))
  (check (pi-retry-config-enabled (pi-settings-retry s)) => #t)
  (check (pi-settings-default-tools s) => #f)
  (check (pi-settings-session-dir s) => #f)
  (check (pi-settings-hide-thinking-block s) => #f)
  (check (pi-settings-model-thinking-levels s) => '())
  (check (pi-settings-enable-skill-commands s) => #t)
) ;let

;; 3. 已是 record 原样返回
(let* ((orig (pi-settings))
       (out (json->pi-settings orig))
      ) ;
  (check (eq? out orig) => #t)
) ;let*

(check-report)
