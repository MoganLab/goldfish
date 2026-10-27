(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code modes interactive worker))

(check-set-mode! 'report-failed)

(define TMP-AGENT-DIR
  (path->string (path-join (path-temp-dir) "worker-job-cfg-test")))

;; 清理旧运行残留，保证测试可重复
(when (file-exists? TMP-AGENT-DIR)
  (rmtree TMP-AGENT-DIR))

(path-mkdir TMP-AGENT-DIR :parents #t :exist-ok #t)
(path-write-text (path->string (path-join (path TMP-AGENT-DIR) (path "auth.json")))
  "{\"google\":{\"type\":\"api_key\",\"key\":\"fixture-google-key\"},\"anthropic\":{\"type\":\"api_key\",\"key\":\"fixture-anthropic-key\"}}")

(let* ((env (list (cons "PI_CODING_AGENT_DIR" TMP-AGENT-DIR)))
       (parsed (parse-pi-args '()))
       (cfg (resolve-pi-config parsed env)))
  ;; 4 元素 job 应用 model + thinking 覆盖
  (let ((cfg* (%worker-job-cfg cfg (list "hi" "high" "anthropic" "claude-haiku-4-5"))))
    (check (cdr (assoc 'thinking-level cfg*)) => "high")
    (check (cdr (assoc 'provider cfg*)) => "anthropic")
    (check (cdr (assoc 'model-id cfg*)) => "claude-haiku-4-5")
    (check (cdr (assoc 'api-key cfg*)) => "fixture-anthropic-key"))

  ;; 旧 2 元素 job 格式向后兼容：仅覆盖 thinking
  (let ((cfg* (%worker-job-cfg cfg (list "hi" "high"))))
    (check (cdr (assoc 'thinking-level cfg*)) => "high")
    (check (cdr (assoc 'provider cfg*)) => "google")
    (check (cdr (assoc 'model-id cfg*)) => (cdr (assoc 'model-id cfg)))))

(rmtree TMP-AGENT-DIR)

(check-report)
