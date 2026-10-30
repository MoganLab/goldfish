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

;; pi-save-hide-thinking-block!：Ctrl+T 切换后持久化 hideThinkingBlock，
;; 只更新该键，保留其他设置（对齐 pi-save-default-thinking-level! 模式）
(define AGENT-DIR (path->string (path-join (path-temp-dir) "pi-save-hide-thinking-test")))
(define SETTINGS (path->string (path-join AGENT-DIR "settings.json")))

(when (file-exists? AGENT-DIR)
  (rmtree AGENT-DIR #t))
(path-mkdir AGENT-DIR :parents #t :exist-ok #t)
(path-write-text SETTINGS
  "{\"defaultProvider\":\"google\",\"defaultThinkingLevel\":\"off\",\"compaction\":{\"enabled\":false}}"
) ;path-write-text

;; 隐藏：写入 true，其他键保留
(pi-save-hide-thinking-block! AGENT-DIR #t)
(let ((json (string->json (path-read-text SETTINGS))))
  (check (cdr (assoc "hideThinkingBlock" json)) => 'true)
  (check (cdr (assoc "defaultProvider" json)) => "google")
  (check (cdr (assoc "defaultThinkingLevel" json)) => "off")
  (check (cdr (assoc "enabled" (cdr (assoc "compaction" json)))) => 'false)
  (check (pi-settings-hide-thinking-block (pi-load-settings AGENT-DIR)) => #t)
) ;let

;; 显示：替换为 false 而不是添加重复键
(pi-save-hide-thinking-block! AGENT-DIR #f)
(let ((json (string->json (path-read-text SETTINGS))))
  (check (cdr (assoc "hideThinkingBlock" json)) => 'false)
  (check (length (filter (lambda (entry) (equal? (car entry) "hideThinkingBlock")) json)) => 1)
  (check (pi-settings-hide-thinking-block (pi-load-settings AGENT-DIR)) => #f)
) ;let

(rmtree AGENT-DIR #t)

(check-report)
