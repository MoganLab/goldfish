(import (scheme base)
  (scheme file)
  (liii check)
  (liii string)
  (liii list)
  (liii tui)
  (liii go)
  (liii shutil)
  (liii path)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code core session-manager)
  (liii pi code core session-name)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

;; 1. /name 不触发 agent，经 worker 持久化并同步 footer；无参数时查询当前名称
(let* ((tmp-agent-dir (path->string (path-join (path-temp-dir) "pi-app-session-name-test")))
       (parsed (parse-pi-args '()))
       (cfg (resolve-pi-config parsed (list (cons "PI_CODING_AGENT_DIR" tmp-agent-dir))))
       (received '())
       (vterm (make-virtual-terminal :width 120 :height 40))
       (app (make-pi-app :terminal vterm
              :handler (lambda (a msg) (set! received (cons msg received)))))
       (sfile-box (vector #f)))
  (when (file-exists? tmp-agent-dir)
    (rmtree tmp-agent-dir #t))
  (pi-app-submit! app "/name  Refactor auth module  ")
  (check received => '())
  (check (editor-text (pi-app-editor app)) => "")
  (let ((job (chan-try-recv! (pi-app-jobs-chan app) #f)))
    (check job => '(session_name "Refactor auth module"))
    (pi-worker-handle-job job parsed cfg sfile-box (pi-app-event-chan app) #f))
  (pi-app-poll-events! app)
  (check (pi-app-session-name app) => "Refactor auth module")
  (check (get-session-name (vector-ref sfile-box 0)) => "Refactor auth module")
  (pi-app-render! app)
  (check-true
    (string-contains?
      (string-join (map strip-ansi (alt-screen-lines (pi-app-screen app))) "\n")
      "Session name set: Refactor auth module"))
  (check-true
    (string-contains?
      (string-join (map strip-ansi (alt-screen-lines (pi-app-screen app))) "\n")
      "• Refactor auth module"))
  (pi-app-submit! app "/name")
  (pi-app-render! app)
  (check-true
    (string-contains?
      (string-join (map strip-ansi (alt-screen-lines (pi-app-screen app))) "\n")
      "Session name: Refactor auth module"))
  (check received => '())
  (rmtree tmp-agent-dir #t))

;; 2. 未命名时 /name 显示用法，且不创建 agent 任务
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (received '())
       (app (make-pi-app :terminal vterm
              :handler (lambda (a msg) (set! received (cons msg received))))))
  (pi-app-submit! app "/name")
  (pi-app-render! app)
  (check-true
    (string-contains?
      (string-join (map strip-ansi (alt-screen-lines (pi-app-screen app))) "\n")
      "Usage: /name <name>"))
  (check received => '())
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f))

;; 3. /session 不触发 agent，经 worker 读取统计并以信息卡片展示核心区块
(let* ((tmp-agent-dir (path->string (path-join (path-temp-dir) "pi-app-session-stats-test")))
       (parsed (parse-pi-args '()))
       (cfg (resolve-pi-config parsed (list (cons "PI_CODING_AGENT_DIR" tmp-agent-dir))))
       (received '())
       (vterm (make-virtual-terminal :width 120 :height 40))
       (app (make-pi-app :terminal vterm
              :handler (lambda (a msg) (set! received (cons msg received)))))
       (sfile-box (vector #f)))
  (when (file-exists? tmp-agent-dir)
    (rmtree tmp-agent-dir #t))
  (pi-app-submit! app "/session")
  (check received => '())
  (check (editor-text (pi-app-editor app)) => "")
  (check (pi-app-chat app) => '())
  (let ((job (chan-try-recv! (pi-app-jobs-chan app) #f)))
    (check job => '(session_stats))
    (pi-worker-handle-job job parsed cfg sfile-box (pi-app-event-chan app) #f))
  (pi-app-poll-events! app)
  (pi-app-render! app)
  (check (length (pi-app-chat app)) => 1)
  (let* ((sfile (vector-ref sfile-box 0))
         ;; 统计卡片可能超出视口高度，这里直接检查卡片内容本身
         (card-lines
           (append-map (lambda (card) (component-render card 120))
             (pi-app-chat app)
           ) ;append-map
         ) ;card-lines
         (text (string-join (map strip-ansi card-lines) "\n"))
         ;; 卡片正文超宽时按显示宽度折行（长路径会被拆到两行），
         ;; 去掉换行后再核对路径，避免折行影响断言
         (flat (string-join (map strip-ansi card-lines) ""))
        ) ;
    (check-true (string? sfile))
    (check-true (string-contains? text "Session Info"))
    (check-true (string-contains? flat "File:"))
    (check-true (string-contains? flat sfile))
    (check-true (string-contains? text "Messages"))
    (check-true (string-contains? text "Total: 0"))
    (check-true (string-contains? text "Tokens"))
    (check-true (string-contains? text "Input: 0"))
    (check (string-contains? text "Cost") => #f))
  (check received => '())
  (rmtree tmp-agent-dir #t))

(check-report)
