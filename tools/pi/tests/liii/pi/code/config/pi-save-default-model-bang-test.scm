;; pi-save-default-model!
;; 将默认 provider 与模型写回 settings.json，保留其他设置。
;;
;; 语法
;; ----
;; (pi-save-default-model! config-dir provider model-id)
;;
;; 参数
;; ----
;; config-dir : string
;; 包含 settings.json 的配置目录。
;; provider : string
;; 默认 provider 名称。
;; model-id : string
;; 默认模型标识。
;;
;; 返回值
;; ----
;; undefined
;; 写入完成后返回未定义值。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii json)
  (liii list)
  (liii path)
  (liii shutil)
  (liii pi code config)
  (liii pi ai config record)
) ;import

(check-set-mode! 'report-failed)

(define AGENT-DIR (path->string (path-join (path-temp-dir) "pi-save-model-test")))
(define SETTINGS (path->string (path-join AGENT-DIR "settings.json")))

(when (file-exists? AGENT-DIR)
  (rmtree AGENT-DIR #t))
(path-mkdir AGENT-DIR :parents #t :exist-ok #t)
(path-write-text SETTINGS
  "{\"defaultProvider\":\"google\",\"defaultModel\":\"gemini-3.8-flash\",\"defaultThinkingLevel\":\"off\",\"compaction\":{\"enabled\":false}}"
) ;path-write-text

;; Ctrl+S 只更新 defaultProvider/defaultModel，保留其他设置。
(pi-save-default-model! AGENT-DIR "anthropic" "claude-haiku-4-5")
(let ((json (string->json (path-read-text SETTINGS))))
  (check (cdr (assoc "defaultProvider" json)) => "anthropic")
  (check (cdr (assoc "defaultModel" json)) => "claude-haiku-4-5")
  (check (cdr (assoc "defaultThinkingLevel" json)) => "off")
  (check (cdr (assoc "enabled" (cdr (assoc "compaction" json)))) => 'false)
) ;let

;; 再次保存替换旧值而不是添加重复键。
(pi-save-default-model! AGENT-DIR "zai" "glm-5")
(let ((json (string->json (path-read-text SETTINGS))))
  (check (cdr (assoc "defaultProvider" json)) => "zai")
  (check (cdr (assoc "defaultModel" json)) => "glm-5")
  (check (length (filter (lambda (entry) (equal? (car entry) "defaultProvider")) json)) => 1)
  (check (length (filter (lambda (entry) (equal? (car entry) "defaultModel")) json)) => 1)
) ;let

(rmtree AGENT-DIR #t)

(check-report)
