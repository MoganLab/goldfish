;; pi-worker-handle-job
;; pi-worker-handle-job 的相关操作与逻辑实现。
;;
;; 语法
;; ----
;; (pi-worker-handle-job ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; any
;; 视具体调用结果而定。

(import (scheme base)
  (scheme file)
  (scheme write)
  (liii check)
  (liii string)
  (liii list)
  (liii path)
  (liii go)
  (liii shutil)
  (liii pi ai message)
  (liii pi ai models)
  (liii pi ai config record)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code core session-manager)
  (liii pi code core compaction session)
  (liii pi code core session-projection)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

(define TMP-AGENT-DIR
  (path->string (path-join (path-temp-dir) "pi-worker-bash-record-test"))
) ;define

;; 上下文消息统一取自投影入口；状态与诊断本用例不关心

(define (projected-messages entries)
  (let*-values (((msgs _state _diag) (pi-build-session-projection entries)))
    msgs
  ) ;let*-values
) ;define

;; 清理旧运行残留，保证测试可重复
(when (file-exists? TMP-AGENT-DIR)
  (rmtree TMP-AGENT-DIR)
) ;when

(let* ((env (list (cons "PI_CODING_AGENT_DIR" TMP-AGENT-DIR)))
       (parsed (parse-pi-args '()))
       (cfg (resolve-pi-config parsed env))
       (box (vector #f))
       ;; 错误事件通道：record 分支失败时经此上报，便于诊断
       (err-chan (make-chan 16))
      ) ;

  ;; 依次投递三条 bash_record 任务：首条惰性创建会话文件，后续复用
  (for-each (lambda (job) (pi-worker-handle-job job parsed cfg box err-chan #f))
    (list (list 'bash_record "echo hi" "hi" 0)
      (list 'bash_record "echo yo" "yo" 0)
      (list 'bash_record "true" "" 0)
    ) ;list
  ) ;for-each

  (let* ((sfile (vector-ref box 0))
         (entries (if (string? sfile) (pi-read-session-entries sfile) '()))
         (msgs (projected-messages entries))
        ) ;
    ;; 1. 会话文件已创建且三次落盘全部成功：header + 3 条 bashExecution
    ;;    （首条为 session header——回归覆盖 Windows 反斜杠 cwd 转义）
    (check-true (string? sfile))
    (check (length entries) => 4)
    (check (cdr (assoc "type" (car entries))) => "session")
    (check (length msgs) => 3)
    (check (pi-message-role (car msgs)) => "user")
    (check-true (string-contains? (pi-user-message-content (car msgs)) "Ran `echo hi`")
    ) ;check-true
    (check-true (string-contains? (pi-user-message-content (cadr msgs)) "Ran `echo yo`")
    ) ;check-true
    ;; 2. 全程无错误事件：三次落盘（含重试兜底）均成功
    (check (chan-try-recv! err-chan #f) => #f)

    ;; 诊断输出（位于断言之后、报告之前，确保落在 CI stdout 尾部窗口内）
    (display ";; diag sfile: ")
    (write sfile)
    (newline)
    (display ";; diag entry-roles: ")
    (write
      (map
        (lambda (e)
          (let ((m (assoc "message" e)))
            (if m (cdr (assoc "role" (cdr m))) (cdr (assoc "type" e)))
          ) ;let
        ) ;lambda
        entries
      ) ;map
    ) ;write
    (newline)
    (display ";; diag msgs-roles: ")
    (write (map pi-message-role msgs))
    (newline)
    (display ";; diag err-event: ")
    (write (chan-try-recv! err-chan #f))
    (newline)
    (flush-output-port)
  ) ;let*
) ;let*

;; 3. 验证 reload_models 任务刷新 worker 侧的 models-store 与 settings
(let* ((fixture-dir
         (path->string
           (path-absolute (path (if (file-exists? "tests/fixtures/user-config/config-tui")
                                  "tests/fixtures/user-config/config-tui"
                                  "tools/pi/tests/fixtures/user-config/config-tui"
                                ) ;if
                          ) ;path
           ) ;path-absolute
         ) ;path->string
       ) ;fixture-dir
       (parsed (parse-pi-args '()))
       (initial-cfg (list (cons 'provider "my-provider")
                      (cons 'model-id "my-model")
                      (cons 'models-store '())
                      (cons 'settings #f)
                      (cons 'config-dir "/nonexistent-old-dir")
                    ) ;list
       ) ;initial-cfg
       (box (vector #f))
       (chan (make-chan 4))
       (new-cfg (pi-worker-handle-job (list 'reload_models fixture-dir)
                  parsed
                  initial-cfg
                  box
                  chan
                  #f
                ) ;pi-worker-handle-job
       ) ;new-cfg
      ) ;
  ;; 保留当前选择的 provider 与 model-id
  (check (cdr (assoc 'provider new-cfg)) => "my-provider")
  (check (cdr (assoc 'model-id new-cfg)) => "my-model")
  ;; models-store 已更新为 fixture 目录下的配置（包含 custom-kimi、google、kimi-coding）
  (let* ((store (cdr (assoc 'models-store new-cfg)))
         (p-names (pi-catalog-provider-names store))
        ) ;
    (check-true (pair? p-names))
    (check-true (and (member "custom-kimi" p-names) #t))
    (check-true (and (member "kimi-coding" p-names) #t))
  ) ;let*
  ;; settings 已更新
  (let ((settings (cdr (assoc 'settings new-cfg))))
    (check-true (pi-settings? settings))
    (check (pi-settings-default-provider settings) => "kimi-coding")
    (check (pi-settings-default-model settings) => "k3.1-pro")
  ) ;let
) ;let*

(check-report)
