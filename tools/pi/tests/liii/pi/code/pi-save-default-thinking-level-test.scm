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

(define AGENT-DIR (path->string (path-join (path-temp-dir) "pi-save-thinking-test")))
(define SETTINGS (path->string (path-join AGENT-DIR "settings.json")))

(when (file-exists? AGENT-DIR)
  (rmtree AGENT-DIR #t))
(path-mkdir AGENT-DIR :parents #t :exist-ok #t)
(path-write-text SETTINGS
  "{\"defaultProvider\":\"google\",\"defaultThinkingLevel\":\"off\",\"compaction\":{\"enabled\":false}}"
) ;path-write-text

;; Ctrl+S 只更新 defaultThinkingLevel，保留其他设置。
(pi-save-default-thinking-level! AGENT-DIR "high")
(let ((json (string->json (path-read-text SETTINGS))))
  (check (cdr (assoc "defaultThinkingLevel" json)) => "high")
  (check (cdr (assoc "defaultProvider" json)) => "google")
  (check (cdr (assoc "enabled" (cdr (assoc "compaction" json)))) => 'false)
  (check (pi-settings-default-thinking-level (pi-load-settings AGENT-DIR)) => "high")
) ;let

;; 再次保存替换旧值而不是添加重复键。
(pi-save-default-thinking-level! AGENT-DIR "low")
(let ((json (string->json (path-read-text SETTINGS))))
  (check (cdr (assoc "defaultThinkingLevel" json)) => "low")
  (check (length (filter (lambda (entry) (equal? (car entry) "defaultThinkingLevel")) json)) => 1)
) ;let

(rmtree AGENT-DIR #t)

(check-report)
