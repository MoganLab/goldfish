(import (scheme base)
  (scheme file)
  (scheme time)
  (liii check)
  (liii unicode)
  (liii string)
  (liii tui)
  (liii go)
  (liii either)
  (liii time)
  (liii subprocess)
  (liii shutil)
  (liii tempfile)
  (liii path)
  (liii fs-watch)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive footer)
  (liii pi code modes interactive common)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code core session-manager)
  (liii pi code core session-name)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

;; 1. 测试首屏组件布局与总行数
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm))
       (screen (pi-app-screen app))
      ) ;
  (pi-app-render! app)
  (let* ((lines (screen-lines screen)) (plain-lines (map strip-ansi lines)))
    ;; 初始应包含 Header(6行) + Editor(3行) + Footer(2行) = 11 行
    (check (length lines) => 11)
    ;; Header
    (check (list-ref plain-lines 0) => "")
    (check-true (string-contains? (list-ref plain-lines 1) " gf v"))
    ;; Editor 分隔线
    (check (list-ref plain-lines 6) => (utf8-make-string 100 #\─))
    (check (string-trim (list-ref plain-lines 7)) => "")
    (check (list-ref plain-lines 8) => (utf8-make-string 100 #\─))
    ;; Footer
    (check-true (string-contains? (list-ref plain-lines 10) "(auto)"))
  ) ;let*
) ;let*

;; 2. 测试消息提交与响应追加（注入 echo handler，与生产后台 worker 路径隔离）
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (echo-handler
         (lambda (app msg)
           (pi-app-add-assistant-text! app (string-append "处理完成: " msg))
         ) ;lambda
       ) ;echo-handler
       (app (make-pi-app :terminal vterm :handler echo-handler))
       (screen (pi-app-screen app))
       (editor (pi-app-editor app))
      ) ;
  (pi-app-render! app)
  ;; 在 Editor 中输入字符
  (component-handle-input editor "ping")
  ;; 按回车触发提交
  (component-handle-input editor 'enter)
  (pi-app-render! app)
  (let ((lines (screen-lines screen)))
    ;; 用户卡片为 4 行（背景空行 + 正文 + 背景空行 + spacer），助手卡片 2 行
    (check (length lines) => 17)
    (check-true (string-contains? (list-ref lines 7) "ping"))
    (check-true (string-contains? (list-ref lines 10) "处理完成: ping"))
    ;; Editor 恢复为空
    (check (editor-text editor) => "")
  ) ;let
) ;let*

;; 3. 测试中文编辑时光标物理列宽定位（全角中文占据 2 列物理宽度）
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (dummy-handler (lambda (app msg) #t))
       (app (make-pi-app :terminal vterm :handler dummy-handler))
       (editor (pi-app-editor app))
      ) ;
  (pi-app-render! app)
  ;; 初始光标在第 7 行（即输入行），列为 0
  (check (terminal-cursor vterm) => '(0 7))

  ;; 1) 输入中文字符 "你好" (2个全角字，物理列宽为 4)
  (component-handle-input editor "你好")
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(4 7))

  ;; 2) 光标向左移动 1 次（移到 "你" 和 "好" 之间，物理列宽为 2）
  (component-handle-input editor 'left)
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(2 7))

  ;; 3) 在中间插入英文 "Hi"（"你Hi好"，光标在 "Hi" 后面，物理列宽为 2 + 2 = 4）
  (component-handle-input editor "Hi")
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(4 7))
  (check (editor-text editor) => "你Hi好")

  ;; 4) 在中间插入中文 "世界"（"你Hi世界好"，光标在 "世界" 后面，物理列宽为 4 + 4 = 8）
  (component-handle-input editor "世界")
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(8 7))
  (check (editor-text editor) => "你Hi世界好")

  ;; 5) 按退格键删除 "界"（"你Hi世好"，物理列宽从 8 变为 6）
  (component-handle-input editor 'backspace)
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(6 7))
  (check (editor-text editor) => "你Hi世好")

  ;; 6) 按 Delete 键向后删除 "好"（"你Hi世"，光标位置保持 6，行尾缩减）
  (component-handle-input editor 'delete)
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(6 7))
  (check (editor-text editor) => "你Hi世")

  ;; 7) 按 Home 键回到行首（物理列宽 0）
  (component-handle-input editor 'home)
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(0 7))

  ;; 8) 按 End 键回到行尾（物理列宽 6）
  (component-handle-input editor 'end)
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(6 7))
) ;let*

;; 4. 测试多轮对话与多行 Markdown 助手回复时，光标精准位于输入框行（不受 \n 干扰）
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (dummy-handler (lambda (app msg) #t))
       (app (make-pi-app :terminal vterm :handler dummy-handler))
       (editor (pi-app-editor app))
      ) ;
  ;; 模拟第一轮对话完成：助手回复包含多行文本（含多个 \n）
  (pi-app-add-user-message! app "第一轮提问")
  (pi-app-add-assistant-text! app "第一行回答\n\n第二行关键回答\n第三行总结")
  (pi-app-render! app)

  ;; Header: 6行 (0..5)
  ;; User Card: 4行 (6..9)：背景空行 + 正文 + 背景空行 + spacer
  ;; Assistant Card: 5行 (10..14)
  ;; Editor Top-Border: 行 15
  ;; Editor Content: 行 16
  ;; Editor Bottom-Border: 行 17
  ;; Footer: 行 18..19
  ;; 光标应位于输入行 (0, 16)，绝不能因 \n 漏算而向上漂移
  (check (terminal-cursor vterm) => '(0 16))

  ;; 第二轮输入文本 "测试第二轮"
  (component-handle-input editor "测试第二轮")
  (pi-app-render! app)
  ;; 5个汉字，物理列宽 10，行号仍为 16
  (check (terminal-cursor vterm) => '(10 16))

  ;; 按回车提交第二轮
  (component-handle-input editor 'enter)
  (pi-app-render! app)
  ;; 第二轮用户卡片追加 4 行 (行 15..18)，输入行平移至行 20
  (check (terminal-cursor vterm) => '(0 20))

  ;; 模拟第二轮助手完成回复
  (pi-app-add-assistant-text! app "第二轮回复：已完成测试")
  (pi-app-render! app)
  ;; Assistant 2 追加 2 行 (行 19..20)，输入行平移至行 22
  (check (terminal-cursor vterm) => '(0 22))

  ;; 第三轮输入 "第三轮验证"
  (component-handle-input editor "第三轮验证")
  (pi-app-render! app)
  ;; 5个中文字符，物理宽度 10，行号 22
  (check (terminal-cursor vterm) => '(10 22))

  ;; 按退格键删除 2 个字（剩 "第三轮"，物理宽度 6）
  (component-handle-input editor 'backspace)
  (component-handle-input editor 'backspace)
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(6 22))
  (check (editor-text editor) => "第三轮")
) ;let*

;; 5. 测试 make-pi-app 透传 provider/model 至 footer，右下角显示实际使用的供应商与模型
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (dummy-handler (lambda (app msg) #t))
       (app (make-pi-app :terminal vterm :handler dummy-handler
              :provider "kimi-coding"
              :model "kimi-for-coding • medium"
            ) ;make-pi-app
       ) ;app
       (screen (pi-app-screen app))
      ) ;let*
  (pi-app-render! app)
  (let* ((lines (screen-lines screen)) (plain-lines (map strip-ansi lines)))
    ;; Footer 第二行应包含透传的 provider 与 model
    (check-true (string-contains? (list-ref plain-lines 10) "(kimi-coding) kimi-for-coding • medium"))
  ) ;let*
) ;let*

;; 6. branch_changed 事件驱动 footer 左下角分支实时更新
;;    （模拟 (liii fs-watch) worker 会话经 event-chan 发来的信号）
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (footer (%pi-app-footer app))
      ) ;let*
  (pi-footer-set-branch! footer "zzz-stale-branch")
  (pi-app-render! app)
  (check-true (string-contains?
                (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")
                "(zzz-stale-branch)"))
  ;; 投递 branch_changed 事件并轮询，footer 应刷新为当前真实分支
  (chan-send! (pi-app-event-chan app) '(branch_changed) 1000)
  (pi-app-poll-events! app)
  (check (pi-footer-branch footer) => (detect-git-branch))
) ;let*

;; 7. 端到端（真实 watcher + 真实事件轮询）：临时 git 仓库切分支，
;;    footer 左下角应实时更新为切换后的分支名
;; %dbg 显式 flush：子进程 stdout 在捕获管道下全缓冲，
;; 必须强制刷新才能在 CI 上定位真实卡点
(define (%dbg msg)
  (display msg)
  (newline)
  (flush-output-port (current-output-port))
) ;define

(let ((git? (either-right? (run-either '("git" "--version")
                            :stdout 'discard :stderr 'discard :timeout 10))))
  (when git?
    (%dbg "[dbg7] begin")
    (let* ((dir (make-temp-dir "gf-pi-appwatch-"))
           (vterm (make-virtual-terminal :width 120 :height 40))
           (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
           (footer (%pi-app-footer app))
          ) ;let*
      (%dbg "[dbg7] app created")
      (check-true
        (either-right?
          (run-either (list "git" "init" dir)
            :stdout 'discard :stderr 'discard :timeout 30)))
      (%dbg "[dbg7] git init done")
      ;; 监听临时仓库（cwd 仍是当前仓库，故直接指定 git-dir）
      (let ((obs (make-branch-watcher (pi-app-event-chan app)
                   (string-append dir "/.git"))))
        (check-true (fs-observer? obs))
        (%dbg "[dbg7] watcher started")
        (pi-footer-set-branch! footer "before-switch")
        (check-true
          (either-right?
            (run-either (list "git" "-C" dir "checkout" "-b" "feature-y")
              :stdout 'discard :stderr 'discard :timeout 30)))
        (%dbg "[dbg7] checkout done")
        ;; 限时轮询事件循环，等待 footer 更新（branch_changed 事件携带临时
        ;; 仓库的 git-dir，主会话直接读其 HEAD 文件解析分支）
        (let ((updated
                (let ((deadline (+ (current-second) 15)))
                  (let loop
                    ((n 0))
                    (when (zero? (modulo n 20))
                      (%dbg (string-append "[dbg7]   poll iter=" (object->string n))))
                    (pi-app-poll-events! app)
                    (if (not (string=? (pi-footer-branch footer) "before-switch"))
                      #t
                      (if (> (current-second) deadline)
                        #f
                        (begin (sleep 0.05) (loop (+ n 1)))
                      ) ;if
                    ) ;if
                  ) ;let loop
                ) ;let
              ) ;updated
           ) ;
          (%dbg "[dbg7] poll loop done")
          (check-true updated)
          (check (pi-footer-branch footer) => "feature-y")
          (let ((b (pi-footer-branch footer)))
            (when b
              (pi-app-render! app)
              (check-true
                (string-contains?
                  (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")
                  (string-append "(" b ")")))
            ) ;when
          ) ;let
        ) ;let
        (stop-branch-watcher! obs)
        (%dbg "[dbg7] stopped")
      ) ;let
      (rmtree dir #t)
      (%dbg "[dbg7] rmtree done")
    ) ;let*
  ) ;when
) ;let

;; 8. CLI 初始 prompt 应按参数顺序依次进入同一个 TUI 会话
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (received '())
       (app (make-pi-app :terminal vterm
              :handler (lambda (a msg) (set! received (cons msg received)))
            ) ;make-pi-app
       ) ;app
      ) ;
  (pi-app-submit-initial! app "I am a frog" '("who am I"))
  (check (reverse received) => '("I am a frog" "who am I"))
) ;let*

;; 9. /name 不触发 agent，经 worker 持久化并同步 footer；无参数时查询当前名称
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
      (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")
      "Session name set: Refactor auth module"))
  (check-true
    (string-contains?
      (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")
      "• Refactor auth module"))
  (pi-app-submit! app "/name")
  (pi-app-render! app)
  (check-true
    (string-contains?
      (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")
      "Session name: Refactor auth module"))
  (check received => '())
  (rmtree tmp-agent-dir #t))

;; 10. 未命名时 /name 显示用法，且不创建 agent 任务
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (received '())
       (app (make-pi-app :terminal vterm
              :handler (lambda (a msg) (set! received (cons msg received))))))
  (pi-app-submit! app "/name")
  (pi-app-render! app)
  (check-true
    (string-contains?
      (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")
      "Usage: /name <name>"))
  (check received => '())
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => #f))

;; 11. /session 不触发 agent，经 worker 读取统计并以信息卡片展示核心区块
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
         (text (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")))
    (check-true (string? sfile))
    (check-true (string-contains? text "Session Info"))
    (check-true (string-contains? text (string-append "File: " sfile)))
    (check-true (string-contains? text "Messages"))
    (check-true (string-contains? text "Total: 0"))
    (check-true (string-contains? text "Tokens"))
    (check-true (string-contains? text "Input: 0"))
    (check (string-contains? text "Cost") => #f))
  (check received => '())
  (rmtree tmp-agent-dir #t))

(%dbg "[dbg7] ALL-DONE")

(check-report)
