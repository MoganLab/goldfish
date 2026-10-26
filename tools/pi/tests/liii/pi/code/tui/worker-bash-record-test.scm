(import (scheme base)
        (scheme file)
        (liii check)
        (liii string)
        (liii list)
        (liii path)
        (liii shutil)
        (liii pi ai message)
        (liii pi code args)
        (liii pi code config)
        (liii pi code session)
        (liii pi code compact session)
        (liii pi code tui worker))

(check-set-mode! 'report-failed)

(define TMP-AGENT-DIR
  (path->string (path-join (path-temp-dir) "pi-worker-bash-record-test"))
) ;define

;; 清理旧运行残留，保证测试可重复
(when (file-exists? TMP-AGENT-DIR) (rmtree TMP-AGENT-DIR))

(let* ((env (list (cons "PI_CODING_AGENT_DIR" TMP-AGENT-DIR)))
       (parsed (parse-pi-args '()))
       (cfg (resolve-pi-config parsed env))
       (box (vector #f))
      ) ;

  ;; 1. 第一条 bash_record 任务：惰性创建会话文件并写入 bashExecution 条目
  (pi-worker-handle-job (list 'bash_record "echo hi" "hi" 0) parsed cfg box #f #f)
  (let* ((sfile (vector-ref box 0))
         (entries (pi-read-session-entries sfile))
         (msgs (pi-project-session-messages entries))
        ) ;
    (check-true (string? sfile))
    ;; header + 1 条 bashExecution 消息
    (check (length entries) => 2)
    (check (length msgs) => 1)
    (check (pi-message-role (car msgs)) => "user")
    (check-true (string-contains? (pi-user-message-content (car msgs)) "Ran `echo hi`"))
  ) ;let*

  ;; 2. 第二条 bash_record 任务：复用同一会话文件（TUI 会话连续性）
  (pi-worker-handle-job (list 'bash_record "echo yo" "yo" 0) parsed cfg box #f #f)
  (let* ((sfile2 (vector-ref box 0))
         (entries (pi-read-session-entries sfile2))
         (msgs (pi-project-session-messages entries))
        ) ;
    (check sfile2 => (vector-ref box 0))
    (check (length entries) => 3)
    (check (length msgs) => 2)
    (check-true (string-contains? (pi-user-message-content (cadr msgs)) "Ran `echo yo`"))
  ) ;let*

  ;; 3. 第三条 bash_record：继续复用同一会话文件，不新建
  (pi-worker-handle-job (list 'bash_record "true" "" 0) parsed cfg box #f #f)
  (check (length (pi-read-session-entries (vector-ref box 0))) => 4)
) ;let*

(check-report)
