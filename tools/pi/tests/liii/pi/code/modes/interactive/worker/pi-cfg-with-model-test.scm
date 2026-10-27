(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii pi ai config record)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code modes interactive worker))

(check-set-mode! 'report-failed)

(define TMP-AGENT-DIR
  (path->string (path-join (path-temp-dir) "pi-cfg-with-model-test")))

;; 清理旧运行残留，保证测试可重复
(when (file-exists? TMP-AGENT-DIR)
  (rmtree TMP-AGENT-DIR))

(path-mkdir TMP-AGENT-DIR :parents #t :exist-ok #t)
(path-write-text (path->string (path-join (path TMP-AGENT-DIR) (path "auth.json")))
  "{\"google\":{\"type\":\"api_key\",\"key\":\"fixture-google-key\"},\"anthropic\":{\"type\":\"api_key\",\"key\":\"fixture-anthropic-key\"}}")

(let* ((env (list (cons "PI_CODING_AGENT_DIR" TMP-AGENT-DIR)))
       (parsed (parse-pi-args '()))
       (cfg (resolve-pi-config parsed env)))
  ;; 跨 provider 切换：
  ;; 覆盖 provider/model-id/model-meta/api-key/supports-thinking，其余键不变
  (let ((cfg* (pi-cfg-with-model cfg "anthropic" "claude-haiku-4-5")))
    (check (cdr (assoc 'provider cfg*)) => "anthropic")
    (check (cdr (assoc 'model-id cfg*)) => "claude-haiku-4-5")
    (check (cdr (assoc 'api-key cfg*)) => "fixture-anthropic-key")
    (check-true (pi-model-metadata? (cdr (assoc 'model-meta cfg*))))
    (check (pi-model-metadata-id (cdr (assoc 'model-meta cfg*))) => "claude-haiku-4-5")
    ;; 原 cfg 不被破坏
    (check (cdr (assoc 'provider cfg)) => "google")
    (check (cdr (assoc 'model-id cfg)) => "gemini-3.8-flash"))

  ;; 同 provider 内切换：仅覆盖 model-id/model-meta
  (let ((cfg* (pi-cfg-with-model cfg "google" "gemini-2.5-pro")))
    (check (cdr (assoc 'provider cfg*)) => "google")
    (check (cdr (assoc 'model-id cfg*)) => "gemini-2.5-pro")
    (check (cdr (assoc 'api-key cfg*)) => "fixture-google-key")))

(rmtree TMP-AGENT-DIR)

(check-report)
