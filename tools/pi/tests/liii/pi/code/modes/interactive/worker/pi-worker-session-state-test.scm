(import (scheme base)
  (scheme file)
  (liii check)
  (liii string)
  (liii list)
  (liii path)
  (liii go)
  (liii shutil)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code core session-manager)
  (liii pi code core compaction session)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

(define TMP-AGENT-DIR
  (path->string (path-join (path-temp-dir) "pi-worker-session-state-test"))
) ;define

;; 清理旧运行残留，保证测试可重复
(when (file-exists? TMP-AGENT-DIR)
  (rmtree TMP-AGENT-DIR)
) ;when

(define (find-entry entries type)
  (find (lambda (e) (equal? (cdr (assoc "type" e)) type)) entries)
) ;define

(let* ((env (list (cons "PI_CODING_AGENT_DIR" TMP-AGENT-DIR)))
       (parsed (parse-pi-args '()))
       (cfg (resolve-pi-config parsed env))
       (box (vector #f))
       (ev-chan (make-chan 16))
      ) ;

  ;; 1. session_state 任务惰性创建会话文件并追加状态条目，父链指向已有条目
  (pi-worker-handle-job (list 'session_state
                          "model_change"
                          (list (cons "provider" "zai") (cons "model-id" "glm-5"))
                        ) ;list
    parsed cfg box ev-chan #f
  ) ;pi-worker-handle-job
  (pi-worker-handle-job (list 'session_state
                          "thinking_level_change"
                          (list (cons "level" "off"))
                        ) ;list
    parsed cfg box ev-chan #f
  ) ;pi-worker-handle-job
  (let* ((sfile (vector-ref box 0))
         (entries (pi-read-session-entries sfile))
         (model (find-entry entries "model_change"))
         (thinking (find-entry entries "thinking_level_change"))
        ) ;
    (check-true (string? sfile))
    (check (length entries) => 3)
    (check (cdr (assoc "type" (car entries))) => "session")
    (check (cdr (assoc "provider" model)) => "zai")
    (check (cdr (assoc "model-id" model)) => "glm-5")
    (check (cdr (assoc "level" thinking)) => "off")
    (check (cdr (assoc "parentId" thinking)) => (cdr (assoc "id" model)))
    (check (chan-try-recv! ev-chan #f) => #f)
  ) ;let*
) ;let*

;; 2. --no-session 时 ensure 返回 #f，状态条目静默跳过（不建文件、不发事件）
(let* ((env (list (cons "PI_CODING_AGENT_DIR" TMP-AGENT-DIR)))
       (parsed (parse-pi-args '("--no-session")))
       (cfg (resolve-pi-config parsed env))
       (box (vector #f))
       (ev-chan (make-chan 16))
      ) ;
  (pi-worker-handle-job (list 'session_state
                          "model_change"
                          (list (cons "provider" "zai") (cons "model-id" "glm-5"))
                        ) ;list
    parsed cfg box ev-chan #f
  ) ;pi-worker-handle-job
  (check (vector-ref box 0) => #f)
  (check (chan-try-recv! ev-chan #f) => #f)
) ;let*

;; 3. 追加失败不抛异常：经 event-chan 上报 session_state_error
(let* ((env (list (cons "PI_CODING_AGENT_DIR" TMP-AGENT-DIR)))
       (parsed (parse-pi-args '()))
       (cfg (resolve-pi-config parsed env))
       ;; 目录不是可追加的文件：触发 append 失败路径
       (box (vector (path->string (path-temp-dir))))
       (ev-chan (make-chan 16))
      ) ;
  (pi-worker-handle-job (list 'session_state
                          "thinking_level_change"
                          (list (cons "level" "high"))
                        ) ;list
    parsed cfg box ev-chan #f
  ) ;pi-worker-handle-job
  (let ((ev (chan-try-recv! ev-chan #f)))
    (check (car ev) => 'session_state_error)
    (check-true (string-contains? (cadr ev) "Failed to record session state"))
  ) ;let
) ;let*

(check-report)
