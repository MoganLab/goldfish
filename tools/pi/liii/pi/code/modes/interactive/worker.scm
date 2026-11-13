;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive worker)
  ;; worker 线程通过重新 import 库并求值 procedure-source 执行，调用链内部函数须全部导出
  (export make-tui-emit pi-tui-agent-worker pi-tui-run-worker pi-tui-send-event!
    pi-cfg-with-thinking pi-cfg-with-model %worker-job-cfg %cfg-override
    pi-worker-ensure-session-file! pi-worker-handle-job %assistant-error-text
    %poll-one-injected %pi-worker-poll-cfg %worker-cfg-with-session-id
  ) ;export
  (import (scheme base)
    (scheme write)
    (scheme process-context)
    (liii go)
    (liii list)
    (liii string)
    (liii path)
    (liii pi ai abort)
    (liii pi ai auth)
    (liii pi ai config record)
    (liii pi ai models meta)
    (liii pi ai models route)
    (liii pi code cli args)
    (liii pi code config)
    (liii pi code config user-catalog)
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

    (define (%cfg-override cfg pairs)
      (let ((keys (map car pairs)))
        (append pairs (filter (lambda (p) (not (memq (car p) keys))) cfg))
      ) ;let
    ) ;define

    ;; 轮询一条注入消息：非阻塞拉取并同步回发事件
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

    ;; worker 运行期 cfg：注入 steering / follow-up 轮询回调，同名键一律替换
    (define (%pi-worker-poll-cfg cfg steering-chan follow-up-chan event-chan)
      (%cfg-override cfg
        (list (cons 'get-steering-messages
                (lambda () (%poll-one-injected steering-chan event-chan))
              ) ;cons
          (cons 'get-follow-up-messages
            (lambda () (%poll-one-injected follow-up-chan event-chan))
          ) ;cons
        ) ;list
      ) ;%cfg-override
    ) ;define

    (define (pi-cfg-with-thinking cfg tl . opt-explicit)
      (%cfg-override cfg
        (cons (cons 'thinking-level tl)
          (if (pair? opt-explicit)
            (list (cons 'thinking-level-explicit? (car opt-explicit)))
            (if (assoc 'thinking-level-explicit? cfg)
              (list (assoc 'thinking-level-explicit? cfg))
              '()
            ) ;if
          ) ;if
        ) ;cons
      ) ;%cfg-override
    ) ;define

    ;; 覆盖 cfg 的模型相关项（会话内 /model 切换后下一条消息生效，对齐上游 setModel）
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
            (cons 'provider-api (pi-cfg-provider-api cfg provider))
            (cons 'model-id model-id)
            (cons 'model-meta meta)
            (cons 'api-key api-key)
            (cons 'supports-thinking supports)
            (cons 'proxy-url (pi-cfg-provider-proxy cfg provider))
          ) ;list
        ) ;%cfg-override
      ) ;let*
    ) ;define

    (define (%job-explicit job cfg)
      (if (>= (length job) 5)
        (list-ref job 4)
        (cdr (or (assoc 'thinking-level-explicit? cfg) '(#f . #f)))
      ) ;if
    ) ;define

    (define (%worker-job-cfg cfg job . opt-parsed)
      (let* ((base (pi-cfg-with-thinking cfg (cadr job) (%job-explicit job cfg)))
             (model-job? (and (> (length job) 3) (string? (list-ref job 2)) (string? (list-ref job 3)))
             ) ;model-job?
             (provider (and model-job? (list-ref job 2)))
             (old-provider (cdr (or (assoc 'provider cfg) '(#f . #f))))
            ) ;
        (if (not model-job?)
          base
          (let ((cfg* (pi-cfg-with-model base provider (list-ref job 3))))
            (if (equal? provider old-provider)
              cfg*
              (augment-pi-config cfg* (if (pair? opt-parsed) (car opt-parsed) #f))
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (%worker-cfg-with-session-id cfg session-file)
      (if (string? session-file)
        (%cfg-override cfg (list (cons 'session-id session-file)))
        cfg
      ) ;if
    ) ;define

    (define (%cfg-context-window cfg)
      (let ((meta (cdr (or (assoc 'model-meta cfg) '(#f . #f)))))
        (cond ((pi-model-metadata? meta) (pi-model-metadata-context-window meta))
              ((pair? meta)
               (let ((pair (assoc 'context-window meta)))
                 (if pair (cdr pair) #f)
               ) ;let
              ) ;
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%worker-assert-cred! cfg)
      (let ((m (pi-missing-credential-msg cfg)))
        (when m
          (error 'credential-error
            (string-append m ". Use /login to configure an API key.")
          ) ;error
        ) ;when
      ) ;let
    ) ;define

    (define (pi-tui-run-worker parsed cfg user-text event-chan . opts)
      (let ((session-file (and (pair? opts) (car opts))))
        (catch #t
          (lambda ()
            (%worker-assert-cred! cfg)
            (run-session-flow parsed
              (%worker-cfg-with-session-id cfg session-file)
              user-text
              (make-tui-emit event-chan (%cfg-context-window cfg))
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

    (define (pi-worker-ensure-session-file! parsed cfg sfile-box)
      (unless (vector-ref sfile-box 0)
        (let-values (((sf _sdir) (setup-session-file-with-name parsed cfg (path->string (path-cwd))))
                    ) ;
          (vector-set! sfile-box 0 sf)
        ) ;let-values
      ) ;unless
      (vector-ref sfile-box 0)
    ) ;define

    (define (%worker-reload-cfg cfg job)
      (let* ((dir
               (if (and (> (length job) 1) (cadr job)) (cadr job) (pi-cfg-config-dir cfg))
             ) ;dir
             (dir-str (if (string? dir) dir (path->string dir)))
             (cur-p (cdr (or (assoc 'provider cfg) '(#f . #f))))
             (new-key (and cur-p (pi-resolve-provider-key cur-p (pi-load-auth dir-str))))
            ) ;
        (%cfg-override cfg
          (list (cons 'models-store (pi-build-user-catalog dir-str))
            (cons 'settings (pi-load-settings dir))
            (cons 'api-key new-key)
          ) ;list
        ) ;%cfg-override
      ) ;let*
    ) ;define

    (define (pi-worker-handle-job job parsed cfg sfile-box event-chan abort-chan
             . opt-chans
            ) ;
      (let* ((steering-chan (if (pair? opt-chans) (car opt-chans) #f))
             (follow-up-chan
               (if (and (pair? opt-chans) (pair? (cdr opt-chans))) (cadr opt-chans) #f)
             ) ;follow-up-chan
             (cfg (%pi-worker-poll-cfg cfg steering-chan follow-up-chan event-chan))
             (effective-cfg cfg)
            ) ;
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
                (cond ((pi-args-ref parsed 'no-session)
                       (pi-tui-send-event! event-chan
                         (list 'session_resume_error "Session persistence is disabled (--no-session)")
                       ) ;pi-tui-send-event!
                      ) ;
                      ((not (file-exists? normalized))
                       (pi-tui-send-event! event-chan
                         (list 'session_resume_error
                           (string-append "Session file not found: " normalized)
                         ) ;list
                       ) ;pi-tui-send-event!
                      ) ;
                      (else (vector-set! sfile-box 0 normalized)
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
         ((and (pair? job) (eq? (car job) 'reload_models))
          (catch #t
            (lambda () (set! effective-cfg (%worker-reload-cfg cfg job)))
            (lambda args #f)
          ) ;catch
         ) ;
         (else (pi-abort-arm-chan! abort-chan)
           (let ((job-cfg (%worker-job-cfg cfg job parsed)))
             (set! effective-cfg job-cfg)
             (pi-tui-run-worker parsed
               job-cfg
               (car job)
               event-chan
               (pi-worker-ensure-session-file! parsed cfg sfile-box)
             ) ;pi-tui-run-worker
           ) ;let
         ) ;else
        ) ;cond
        effective-cfg
      ) ;let*
    ) ;define

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
             (sfile-box (vector initial-session-file))
            ) ;
        (set-pi-retry-logger! (lambda (msg) (pi-tui-send-event! event-chan (list 'retry msg)))
        ) ;set-pi-retry-logger!
        (let loop
          ((current-cfg cfg))
          (let ((job (chan-recv! jobs-chan)))
            (when (and job (not (eof-object? job)))
              (loop (pi-worker-handle-job job parsed current-cfg sfile-box
                      event-chan abort-chan steering-chan follow-up-chan
                    ) ;pi-worker-handle-job
              ) ;loop
            ) ;when
          ) ;let
        ) ;let
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
