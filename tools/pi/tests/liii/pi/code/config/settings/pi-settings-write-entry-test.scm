;; pi-settings-write-entry
;; 生成更新 settings.json 的写入条目 (rel . json-text)，保留未知键与其余配置。
;;
;; 语法
;; ----
;; (pi-settings-write-entry config-dir patch-or-settings) -> pair
;;
;; 参数
;; ----
;; config-dir : string
;; JSON 配置目录。
;; patch-or-settings : alist | pi-settings?
;; 待合并的键值 alist 或 <pi-settings> record。
;;
;; 返回值
;; ----
;; pair
;; ("settings.json" . formatted-json-text)。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii json)
  (liii path)
  (liii shutil)
  (liii pi code config settings)
  (liii pi ai config settings)
) ;import

(check-set-mode! 'report-failed)

(define AGENT-DIR
  (path->string (path-join (path-temp-dir) "pi-settings-write-entry-test"))
) ;define

(define SETTINGS (path->string (path-join AGENT-DIR "settings.json")))

(when (file-exists? AGENT-DIR)
  (rmtree AGENT-DIR #t)
) ;when
(path-mkdir AGENT-DIR :parents #t :exist-ok #t)
(path-write-text SETTINGS
  "{\"defaultProvider\":\"google\",\"compaction\":{\"enabled\":false}}"
) ;path-write-text

(let* ((entry (pi-settings-write-entry AGENT-DIR
                (list (cons "defaultProvider" "anthropic") (cons "defaultModel" "claude-haiku-4-5"))
              ) ;pi-settings-write-entry
       ) ;entry
       (parsed (string->json (cdr entry)))
      ) ;
  (check (car entry) => "settings.json")
  (check (cdr (assoc "defaultProvider" parsed)) => "anthropic")
  (check (cdr (assoc "defaultModel" parsed)) => "claude-haiku-4-5")
  (check (cdr (assoc "enabled" (cdr (assoc "compaction" parsed)))) => 'false)
) ;let*

;; 支持直接传入 <pi-settings> record
(let* ((s (pi-settings "deepseek" "deepseek-chat" "high"))
       (entry (pi-settings-write-entry AGENT-DIR s))
       (parsed (string->json (cdr entry)))
      ) ;
  (check (cdr (assoc "defaultProvider" parsed)) => "deepseek")
  (check (cdr (assoc "defaultModel" parsed)) => "deepseek-chat")
  (check (cdr (assoc "defaultThinkingLevel" parsed)) => "high")
  (check (cdr (assoc "enabled" (cdr (assoc "compaction" parsed)))) => 'false)
) ;let*

(rmtree AGENT-DIR #t)

(check-report)
