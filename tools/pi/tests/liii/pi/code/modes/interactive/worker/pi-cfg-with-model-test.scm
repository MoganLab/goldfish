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

;; 跨协议 provider 切换：provider-api 必须同步更新，
;; 否则协议分发沿用旧 provider 的实现，请求会打到错误端点（404）
(let* ((agent-dir (path->string (path-join (path-temp-dir) "pi-cfg-with-model-api-test")))
       (env (list (cons "PI_CODING_AGENT_DIR" agent-dir)))
      ) ;
  (when (file-exists? agent-dir)
    (rmtree agent-dir)
  ) ;when
  (path-mkdir (path agent-dir) :parents #t :exist-ok #t)
  (path-write-text (path->string (path-join (path agent-dir) (path "providers.json")))
    "[\"google\", \"zai-coding-cn\"]")
  (let* ((cfg (resolve-pi-config (parse-pi-args '()) env))
         (api-before (cdr (assoc 'provider-api cfg)))
        ) ;
    ;; 默认 provider 为 google
    (check (cdr (assoc 'provider cfg)) => "google")
    (check api-before => "google-generative-ai")
    ;; 切到 openai-completions 协议的 provider
    (let ((cfg* (pi-cfg-with-model cfg "zai-coding-cn" "glm-5.3")))
      (check (cdr (assoc 'provider cfg*)) => "zai-coding-cn")
      (check (cdr (assoc 'provider-api cfg*)) => "openai-completions")
      (check (pi-model-metadata-base-url (cdr (assoc 'model-meta cfg*)))
        => "https://open.bigmodel.cn/api/coding/paas/v4"
      ) ;check
      ;; 原 cfg 的 provider-api 不被破坏
      (check (cdr (assoc 'provider-api cfg)) => "google-generative-ai"))
    ;; 再切回 google：api 回到 google-generative-ai
    (let ((cfg* (pi-cfg-with-model cfg "google" "gemini-3.8-flash")))
      (check (cdr (assoc 'provider-api cfg*)) => "google-generative-ai")))
  (rmtree agent-dir)
) ;let*

(rmtree TMP-AGENT-DIR)

(check-report)
