;; %worker-job-cfg
;; 根据当前工作任务动态派生与覆盖 Worker 配置。
;;
;; 语法
;; ----
;; (%worker-job-cfg cfg job)
;; (%worker-job-cfg cfg job parsed)
;;
;; 参数
;; ----
;; cfg : list?
;; 当前基础配置列表。
;; job : list?
;; 包含提示词、思考等级以及可选 provider 与 model 的任务元组。
;; parsed : list?
;; 可选的已解析命令行参数。
;;
;; 返回值
;; ----
;; list?
;; 覆盖了相应配置项的新配置列表。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

(define TMP-AGENT-DIR
  (path->string (path-join (path-temp-dir) "worker-job-cfg-test"))
) ;define

;; 清理旧运行残留，保证测试可重复
(when (file-exists? TMP-AGENT-DIR)
  (rmtree TMP-AGENT-DIR)
) ;when

(path-mkdir TMP-AGENT-DIR :parents #t :exist-ok #t)
(path-write-text (path->string (path-join (path TMP-AGENT-DIR) (path "auth.json")))
  "{\"google\":{\"type\":\"api_key\",\"key\":\"fixture-google-key\"},\"anthropic\":{\"type\":\"api_key\",\"key\":\"fixture-anthropic-key\"}}"
) ;path-write-text

(let* ((env (list (cons "PI_CODING_AGENT_DIR" TMP-AGENT-DIR)))
       (parsed (parse-pi-args '()))
       (cfg (resolve-pi-config parsed env))
      ) ;
  ;; 4 元素 job 应用 model + thinking 覆盖
  (let ((cfg* (%worker-job-cfg cfg (list "hi" "high" "anthropic" "claude-haiku-4-5"))))
    (check (cdr (assoc 'thinking-level cfg*)) => "high")
    (check (cdr (assoc 'provider cfg*)) => "anthropic")
    (check (cdr (assoc 'model-id cfg*)) => "claude-haiku-4-5")
    (check (cdr (assoc 'api-key cfg*)) => "fixture-anthropic-key")
  ) ;let

  ;; 旧 2 元素 job 格式向后兼容：仅覆盖 thinking
  (let ((cfg* (%worker-job-cfg cfg (list "hi" "high"))))
    (check (cdr (assoc 'thinking-level cfg*)) => "high")
    (check (cdr (assoc 'provider cfg*)) => "google")
    (check (cdr (assoc 'model-id cfg*)) => (cdr (assoc 'model-id cfg)))
  ) ;let
) ;let*

(rmtree TMP-AGENT-DIR)

(check-report)
