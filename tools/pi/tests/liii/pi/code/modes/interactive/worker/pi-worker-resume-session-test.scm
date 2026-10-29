(import (scheme base)
  (scheme file)
  (scheme write)
  (liii check)
  (liii string)
  (liii list)
  (liii path)
  (liii go)
  (liii time)
  (liii shutil)
  (liii pi ai message)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code core session-manager)
  (liii pi code core session-name)
  (liii pi code core compaction session)
  (liii pi code core session-projection)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

(define TMP-AGENT-DIR
  (path->string (path-join (path-temp-dir) "pi-worker-resume-session-test"))
) ;define

;; 清理旧运行残留，保证测试可重复
(when (file-exists? TMP-AGENT-DIR)
  (rmtree TMP-AGENT-DIR)
) ;when

;; 追加一条用户消息（模拟历史上下文中的“暗号”）

(define (append-user-message! file text)
  (persist-new-messages! file '() (list (pi-user-message text)))
) ;define

;; 会话文件投影出的消息文本（拼接全部 role/text）

(define (projected-text file)
  (let*-values (((msgs _state _diag)
                 (pi-build-session-projection (pi-read-session-entries file))
                ) ;
               ) ;
    (string-join (map (lambda (msg) (object->string msg)) msgs) " ")
  ) ;let*-values
) ;define

(let* ((env (list (cons "PI_CODING_AGENT_DIR" TMP-AGENT-DIR)))
       (parsed (parse-pi-args '()))
       (cfg (resolve-pi-config parsed env))
       (box (vector #f))
       (ev-chan (make-chan 16))
       (cwd (path->string (path-cwd)))
      ) ;

  ;; 构造两个历史会话：A（命名会话，暗号菠萝）与 B（暗号西瓜）
  (let-values (((sfile-a _) (setup-session-file parsed cfg cwd)))
    (let-values (((sfile-b _) (setup-session-file parsed cfg cwd)))
      (append-user-message! sfile-a "暗号是菠萝")
      (append-user-message! sfile-b "暗号是西瓜")
      (append-session-info! sfile-a "会话甲")

      ;; 1. 切换到 A：worker 的会话文件被替换，并回传名称
      (pi-worker-handle-job (list 'session_resume sfile-a) parsed cfg box ev-chan #f)
      (let ((ev (chan-recv! ev-chan)))
        (check (car ev) => 'session_resumed)
        (check (cadr ev) => sfile-a)
        (check (caddr ev) => "会话甲")
      ) ;let
      (check (vector-ref box 0) => sfile-a)

      ;; 2. 切换后下一条消息使用目标文件：A 的上下文生效，B 的上下文不串入
      (check (pi-worker-ensure-session-file! parsed cfg box) => sfile-a)
      (check-true (string-contains? (projected-text sfile-a) "菠萝"))
      (check-false (string-contains? (projected-text sfile-a) "西瓜"))
      (check-false (string-contains? (projected-text sfile-b) "菠萝"))

      ;; 3. 切换后的落盘写入目标文件：/name 记到 A 上，B 不受影响
      (pi-worker-handle-job (list 'session_name "改名后")
        parsed
        cfg
        box
        ev-chan
        #f
      ) ;pi-worker-handle-job
      (let ((ev (chan-recv! ev-chan)))
        (check (car ev) => 'session_name_changed)
        (check (cadr ev) => "改名后")
      ) ;let
      (check (get-session-name sfile-a) => "改名后")
      (check (get-session-name sfile-b) => #f)

      ;; 4. 连续切换：最后一次确认的会话生效，不会写回前一次会话
      (pi-worker-handle-job (list 'session_resume sfile-b) parsed cfg box ev-chan #f)
      (let ((ev (chan-recv! ev-chan)))
        (check (car ev) => 'session_resumed)
        (check (cadr ev) => sfile-b)
      ) ;let
      (check (vector-ref box 0) => sfile-b)
      (pi-worker-handle-job (list 'session_resume sfile-a) parsed cfg box ev-chan #f)
      (let ((ev (chan-recv! ev-chan)))
        (check (car ev) => 'session_resumed)
        (check (cadr ev) => sfile-a)
      ) ;let
      (check (vector-ref box 0) => sfile-a)
      (check (pi-worker-ensure-session-file! parsed cfg box) => sfile-a)

      ;; 5. 目标文件不存在：报错并保留原会话，不创建任何文件
      (let ((missing (string-append sfile-a ".missing")))
        (pi-worker-handle-job (list 'session_resume missing) parsed cfg box ev-chan #f)
        (let ((ev (chan-recv! ev-chan)))
          (check (car ev) => 'session_resume_error)
          (check-true (string-contains? (cadr ev) "not found"))
        ) ;let
        (check (vector-ref box 0) => sfile-a)
        (check (file-exists? missing) => #f)
      ) ;let

      ;; 6. 无错误事件残留
      (check (chan-try-recv! ev-chan #f) => #f)

      ;; 7. --no-session 模式：worker 侧同样拒绝恢复，不切换会话文件
      (let ((parsed-no-session (parse-pi-args '("--no-session")))
            (box-no-session (vector sfile-a))
           ) ;
        (pi-worker-handle-job (list 'session_resume sfile-b)
          parsed-no-session
          cfg
          box-no-session
          ev-chan
          #f
        ) ;pi-worker-handle-job
        (let ((ev (chan-recv! ev-chan)))
          (check (car ev) => 'session_resume_error)
          (check-true (string-contains? (cadr ev) "--no-session"))
        ) ;let
        (check (vector-ref box-no-session 0) => sfile-a)
      ) ;let

      (rmtree TMP-AGENT-DIR #t)
    ) ;let-values
  ) ;let-values
) ;let*

;; 8. 真实后台 worker 线程路径：session_resume 与后续任务串行落到同一文件
;;    （与 TUI 生产路径一致：go worker 消费 jobs-chan）
(let* ((env (list (cons "PI_CODING_AGENT_DIR" TMP-AGENT-DIR)))
       (parsed (parse-pi-args '()))
       (cfg (resolve-pi-config parsed env))
       (jobs (make-chan 16))
       (events (make-chan 16))
       (cwd (path->string (path-cwd)))
      ) ;
  (when (file-exists? TMP-AGENT-DIR)
    (rmtree TMP-AGENT-DIR)
  ) ;when
  (let-values (((sfile _) (setup-session-file parsed cfg cwd)))
    (go (pi-tui-agent-worker jobs events #f #f))
    ;; 先恢复会话，再投递 bash_record：两者必须串行落到同一文件
    (chan-send! jobs (list 'session_resume sfile) 1000)
    (let ((ev (chan-recv! events)))
      (check (car ev) => 'session_resumed)
      (check (cadr ev) => sfile)
    ) ;let
    (chan-send! jobs (list 'bash_record "echo bg-marker" "bg-marker" 0) 1000)
    (sleep 0.5)
    (check-true (string-contains? (path-read-text sfile) "bg-marker"))
    (rmtree TMP-AGENT-DIR #t)
  ) ;let-values
) ;let*

(check-report)
