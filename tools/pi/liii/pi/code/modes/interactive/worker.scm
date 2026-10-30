;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive worker)
  ;; 注意：go worker 线程通过重新 import 库并求值 procedure-source 来执行，
  ;; 只能看到各库的导出绑定，因此 worker 调用链上的内部函数必须全部导出
  (export make-tui-emit pi-tui-agent-worker pi-tui-run-worker pi-tui-send-event!
    pi-cfg-with-thinking pi-cfg-with-model %worker-job-cfg
    pi-worker-ensure-session-file! pi-worker-handle-job %assistant-error-text
    %poll-one-injected
  ) ;export
  (import (scheme base)
    (scheme write)
    (scheme process-context)
    (liii go)
    (liii list)
    (liii string)
    (liii path)
    (liii pi abort)
    (liii pi ai auth)
    (liii pi ai models meta)
    (liii pi ai models route)
    (liii pi code cli args)
    (liii pi code config)
    (liii pi code core session-manager)
    (liii pi code core session-name)
    (liii pi code core session-stats)
    (liii pi code session-state)
    (liii pi ai retry)
    (liii pi code modes interactive worker-emit)
    (liii pi code modes flow)
    (liii pi code modes output)
  ) ;import
  (begin

    ;; 轮询一条注入消息（steering / follow-up 通道共用）：
    ;; 非阻塞拉取；拉到非空文本时先回发 user_message 事件同步主线程显示，
    ;; 再返回单元素列表供 run-agent-loop 注入
    (define (%poll-one-injected chan event-chan)
      (let ((msg
              (and chan (catch #t (lambda () (chan-try-recv! chan #f)) (lambda args #f)))
            ) ;msg
           ) ;
        (if (and (string? msg) (not (string-null? msg)))
          (begin
            (pi-tui-send-event! event-chan (list 'user_message msg))
            (list msg)
          ) ;begin
          '()
        ) ;if
      ) ;let
    ) ;define

    ;; 覆盖 cfg alist 的 thinking-level 项（会话内 shift+tab 切换后生效）
    (define (pi-cfg-with-thinking cfg tl)
      (cons (cons 'thinking-level tl)
        (filter
          (lambda (p) (not (eq? (car p) 'thinking-level)))
          cfg
        ) ;filter
      ) ;cons
    ) ;define

    ;; alist 覆盖：pairs 中的键优先，其余沿用原 cfg
    (define (%cfg-override cfg pairs)
      (let ((keys (map car pairs)))
        (append pairs (filter (lambda (p) (not (memq (car p) keys))) cfg))
      ) ;let
    ) ;define

    ;; 覆盖 cfg 的模型相关项（会话内 /model 切换后下一条消息生效，对齐上游 setModel）：
    ;; 重新解析 provider 凭证与 model 元数据，并按新模型重算 supports-thinking
    (define (pi-cfg-with-model cfg provider model-id)
      (let* ((config-dir (pi-cfg-config-dir cfg))
             (models-store (cdr (or (assoc 'models-store cfg) (cons #f '()))))
             (auth (pi-load-auth config-dir))
             (api-key (pi-resolve-provider-key provider auth))
             (raw-meta (pi-resolve-model-metadata provider model-id models-store))
             (meta (pi-ensure-catalog-meta raw-meta provider model-id models-store))
             (supports (pi-model-supports-thinking? models-store provider model-id))
            ) ;
        (%cfg-override cfg
          (list (cons 'provider provider)
            (cons 'model-id model-id)
            (cons 'model-meta meta)
            (cons 'api-key api-key)
            (cons 'supports-thinking supports)
          ) ;list
        ) ;%cfg-override
      ) ;let*
    ) ;define

    ;; job 载荷 → 任务 cfg：
    ;; - (消息 thinking-level) 旧格式：仅覆盖 thinking（向后兼容）
    ;; - (消息 thinking-level provider model-id)：再叠加 /model 切换
    (define (%worker-job-cfg cfg job)
      (let ((base (pi-cfg-with-thinking cfg (cadr job))))
        (if (and (> (length job) 3) (string? (list-ref job 2)) (string? (list-ref job 3)))
          (pi-cfg-with-model base (list-ref job 2) (list-ref job 3))
          base
        ) ;if
      ) ;let
    ) ;define

    ;; 在 (liii go) 的后台独立 worker 线程中运行，完全不阻塞前台终端编辑与打字；
    ;; session-file（可选）为复用的会话文件，传入时消息始终携带既有上下文
    (define (pi-tui-run-worker parsed cfg user-text event-chan . opts)
      (let ((session-file (and (pair? opts) (car opts))))
        (catch #t
          (lambda ()
            (run-session-flow parsed
              cfg
              user-text
              (make-tui-emit event-chan)
              (lambda (res-msgs) #t)
              session-file
              '()
              #f
            ) ;run-session-flow
          ) ;lambda
          (lambda (tag . err-args)
            (pi-tui-send-event! event-chan
              (list 'error
                (string-append (object->string tag) ": " (object->string err-args))
              ) ;list
            ) ;pi-tui-send-event!
            (pi-tui-send-event! event-chan (list 'agent_settled #f))
          ) ;lambda
        ) ;catch
      ) ;let
    ) ;define

    ;; 惰性解析并复用会话文件（对齐 pi 单 TUI 会话模型）：
    ;; 首个任务（消息、bash 记录或会话命名）时创建/恢复会话文件，后续任务全部复用，
    ;; 跨消息保持上下文连续；--no-session 时恒为 #f
    (define (pi-worker-ensure-session-file! parsed cfg sfile-box)
      (unless (vector-ref sfile-box 0)
        (let-values (((sf _sdir) (setup-session-file-with-name parsed cfg (path->string (path-cwd))))
                    ) ;
          (vector-set! sfile-box 0 sf)
        ) ;let-values
      ) ;unless
      (vector-ref sfile-box 0)
    ) ;define

    ;; 任务分发（对齐 pi 的会话事件处理）：
    ;; - (bash_record 命令 输出 退出码)：! 命令结果写入会话文件，不触发 LLM
    ;;   请求；agent 忙时在 jobs-chan 排队，结算后才落盘（对齐 pi 流式期间
    ;;   pending、agent_end 冲洗的语义）
    ;; - (session_name 名称)：更新会话显示名并写入 session_info，不触发 LLM
    ;; - (session_state 类型 字段)：把模型/思考等级等状态条目写入会话文件，
    ;;   --no-session 时会话文件为 #f，API 静默跳过
    ;; - (session_resume 路径)：把 worker 的会话文件切换到所选 JSONL，
    ;;   后续消息、bash 记录与命名均针对新会话文件
    ;; - (消息 thinking-level)：正常 agent 任务，携带复用的会话文件
    (define (pi-worker-handle-job job parsed cfg sfile-box event-chan abort-chan)
      (cond
       ((and (pair? job) (eq? (car job) 'bash_record))
        (catch #t
          (lambda ()
            (append-bash-execution! (pi-worker-ensure-session-file! parsed cfg sfile-box)
              (cadr job)
              (caddr job)
              (cadddr job)
            ) ;append-bash-execution!
          ) ;lambda
          (lambda (tag . args)
            (pi-tui-send-event! event-chan
              (list 'error
                (string-append "bash record failed: " (object->string (cons tag args)))
              ) ;list
            ) ;pi-tui-send-event!
          ) ;lambda
        ) ;catch
       ) ;
       ((and (pair? job) (eq? (car job) 'session_stats))
        (catch #t
          (lambda ()
            (let* ((sfile (pi-worker-ensure-session-file! parsed cfg sfile-box))
                   (stats (pi-get-session-stats sfile))
                  ) ;
              (pi-tui-send-event! event-chan (list 'session_stats stats))
            ) ;let*
          ) ;lambda
          (lambda (tag . args)
            (pi-tui-send-event! event-chan
              (list 'session_stats_error
                (string-append "Failed to read session stats: "
                  (object->string (cons tag args))
                ) ;string-append
              ) ;list
            ) ;pi-tui-send-event!
          ) ;lambda
        ) ;catch
       ) ;
       ((and (pair? job) (eq? (car job) 'session_name))
        (catch #t
          (lambda ()
            (let ((name (pi-normalize-session-name (cadr job))))
              (append-session-info! (pi-worker-ensure-session-file! parsed cfg sfile-box)
                name
              ) ;append-session-info!
              (pi-tui-send-event! event-chan (list 'session_name_changed name))
            ) ;let
          ) ;lambda
          (lambda (tag . args)
            (pi-tui-send-event! event-chan
              (list 'session_name_error
                (string-append "Failed to set session name: " (object->string (cons tag args)))
              ) ;list
            ) ;pi-tui-send-event!
          ) ;lambda
        ) ;catch
       ) ;
       ((and (pair? job) (eq? (car job) 'session_state))
        (catch #t
          (lambda ()
            (append-session-state-entry! (pi-worker-ensure-session-file! parsed cfg sfile-box)
              (cadr job)
              (caddr job)
            ) ;append-session-state-entry!
          ) ;lambda
          (lambda (tag . args)
            (pi-tui-send-event! event-chan
              (list 'session_state_error
                (string-append "Failed to record session state: "
                  (object->string (cons tag args))
                ) ;string-append
              ) ;list
            ) ;pi-tui-send-event!
          ) ;lambda
        ) ;catch
       ) ;
       ((and (pair? job) (eq? (car job) 'session_resume))
        (catch #t
          (lambda ()
            (let* ((path (cadr job)) (normalized (if (string? path) path (path->string path))))
              (cond
                ;; --no-session 模式：会话持久化已禁用，拒绝恢复（不切换、不建文件）
                ((pi-args-ref parsed 'no-session)
                 (pi-tui-send-event! event-chan
                   (list 'session_resume_error "Session persistence is disabled (--no-session)")
                 ) ;pi-tui-send-event!
                ) ;
                ;; 目标文件不存在：保留原会话并报错，不创建任何新文件
                ((not (file-exists? normalized))
                 (pi-tui-send-event! event-chan
                   (list 'session_resume_error
                     (string-append "Session file not found: " normalized)
                   ) ;list
                 ) ;pi-tui-send-event!
                ) ;
                (else (vector-set! sfile-box 0 normalized)
                  ;; 随事件回传目标会话的投影状态（模型/思考等级），
                  ;; 状态缺失时为 #f，前台据此保留当前值
                  (pi-tui-send-event! event-chan
                    (list 'session_resumed
                      normalized
                      (get-session-name normalized)
                      (pi-session-state-of normalized)
                    ) ;list
                  ) ;pi-tui-send-event!
                ) ;else
              ) ;cond
            ) ;let*
          ) ;lambda
          (lambda (tag . args)
            (pi-tui-send-event! event-chan
              (list 'session_resume_error
                (string-append "Failed to resume session: " (object->string (cons tag args)))
              ) ;list
            ) ;pi-tui-send-event!
          ) ;lambda
        ) ;catch
       ) ;
       (else
         ;; 新任务开始前 arm abort 通道：排空上一轮残留信号并复位标志，
         ;; 本轮任务期间流式回调 / turn 边界 / 工具边界经 pi-aborted? 探测
         (pi-abort-arm-chan! abort-chan)
         (pi-tui-run-worker parsed
           (%worker-job-cfg cfg job)
           (car job)
           event-chan
           (pi-worker-ensure-session-file! parsed cfg sfile-box)
         ) ;pi-tui-run-worker
       ) ;else
      ) ;cond
    ) ;define

    ;; 顶层后台工作协程：并发循环消费 jobs-chan 任务并回传事件到 event-chan；
    ;; jobs 包含用户消息 (text thinking-level [provider model-id])、
    ;; bash 记录与 session_name 命名任务
    ;; steering-chan / follow-up-chan（可选）：运行中注入消息通道，
    ;; 由 run-agent-loop 在轮询点消费，消费时回发 user_message 事件同步显示
    ;; 注意：go 绑定的 let 语法会“求值”绑定值，alist 会被当作函数调用而报错，
    ;; 因此 parsed/cfg 不能跨 go 边界传入，必须在 worker 会话内自行解析
    (define* (pi-tui-agent-worker jobs-chan
               event-chan
               abort-chan
               (initial-session-file #f)
               (steering-chan #f)
               (follow-up-chan #f)
             ) ;pi-tui-agent-worker
      (let* ((parsed (parse-pi-args (cdr (command-line))))
             (cfg (augment-pi-config (resolve-pi-config parsed (get-environment-variables))
                    parsed
                  ) ;augment-pi-config
             ) ;cfg
             ;; 会话文件盒：全生命周期复用同一文件（TUI 会话连续性）
             (sfile-box (vector initial-session-file))
             ;; 注入轮询回调：非阻塞拉取一条消息，拉到时回发 user_message
             ;; 事件（主线程移除待处理显示并上屏用户卡片）；
             ;; 未提供通道或拉取失败时返回空表（循环按无消息处理）
             (poll-cfg
               (append
                 (list (cons 'get-steering-messages
                         (lambda () (%poll-one-injected steering-chan event-chan))
                       ) ;cons
                   (cons 'get-follow-up-messages
                     (lambda () (%poll-one-injected follow-up-chan event-chan))
                   ) ;cons
                 ) ;list
                 cfg
               ) ;append
             ) ;poll-cfg
            ) ;
        ;; 重试与 multikey 换 key 日志共用 retry logger 通道，转为界面卡片
        (set-pi-retry-logger! (lambda (msg) (pi-tui-send-event! event-chan (list 'retry msg)))
        ) ;set-pi-retry-logger!
        (let loop
          ()
          ;; job 消息为 (text thinking-level [provider model-id])、bash_record 或 session_name：
          ;; 会话内切换的 thinking 级别与 /model 模型均对下一条消息生效
          ;; 通道关闭（eof）时结束循环：TUI 退出路径会关闭 jobs-chan，
          ;; chan-recv! 此时返回 eof 对象（eof 为真值），必须显式排除
          (let ((job (chan-recv! jobs-chan)))
            (when (and job (not (eof-object? job)))
              (pi-worker-handle-job job parsed poll-cfg sfile-box event-chan abort-chan)
              (loop)
            ) ;when
          ) ;let
        ) ;let
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
