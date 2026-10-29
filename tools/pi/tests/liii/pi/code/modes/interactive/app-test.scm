(import (scheme base)
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
  (liii fs-watch)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive footer)
  (liii pi code modes interactive common)
) ;import

(check-set-mode! 'report-failed)

;; 1. 测试首屏组件布局与总行数
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm))
       (screen (pi-app-screen app))
      ) ;
  (pi-app-render! app)
  (let* ((lines (alt-screen-lines screen)) (plain-lines (map strip-ansi lines)))
    ;; 全屏帧固定为终端高度（40 行）：
    ;; 对话记录视口 35 行 + Header(6行) + Editor(3行) + Footer(2行)
    (check (length lines) => 40)
    ;; Header
    (check (list-ref plain-lines 0) => "")
    (check-true (string-contains? (list-ref plain-lines 1) " gf v"))
    ;; Editor 分隔线（输入区固定贴在屏幕底部）
    (check (list-ref plain-lines 35) => (utf8-make-string 100 #\─))
    (check (string-trim (list-ref plain-lines 36)) => "")
    (check (list-ref plain-lines 37) => (utf8-make-string 100 #\─))
    ;; Footer（两行：左下角路径/分支，右下角用量与模型）
    (check-true (string-contains? (list-ref plain-lines 39) "(auto)"))
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
  (let ((lines (alt-screen-lines screen)))
    ;; 用户卡片为 4 行（背景空行 + 正文 + 背景空行 + spacer），助手卡片 2 行；
    ;; 全屏帧高度固定为终端高度 40 行
    (check (length lines) => 40)
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
  ;; 初始光标在第 36 行（即输入行，全屏布局下输入区固定在屏幕底部），列为 0
  (check (terminal-cursor vterm) => '(0 36))

  ;; 1) 输入中文字符 "你好" (2个全角字，物理列宽为 4)
  (component-handle-input editor "你好")
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(4 36))

  ;; 2) 光标向左移动 1 次（移到 "你" 和 "好" 之间，物理列宽为 2）
  (component-handle-input editor 'left)
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(2 36))

  ;; 3) 在中间插入英文 "Hi"（"你Hi好"，光标在 "Hi" 后面，物理列宽为 2 + 2 = 4）
  (component-handle-input editor "Hi")
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(4 36))
  (check (editor-text editor) => "你Hi好")

  ;; 4) 在中间插入中文 "世界"（"你Hi世界好"，光标在 "世界" 后面，物理列宽为 4 + 4 = 8）
  (component-handle-input editor "世界")
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(8 36))
  (check (editor-text editor) => "你Hi世界好")

  ;; 5) 按退格键删除 "界"（"你Hi世好"，物理列宽从 8 变为 6）
  (component-handle-input editor 'backspace)
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(6 36))
  (check (editor-text editor) => "你Hi世好")

  ;; 6) 按 Delete 键向后删除 "好"（"你Hi世"，光标位置保持 6，行尾缩减）
  (component-handle-input editor 'delete)
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(6 36))
  (check (editor-text editor) => "你Hi世")

  ;; 7) 按 Home 键回到行首（物理列宽 0）
  (component-handle-input editor 'home)
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(0 36))

  ;; 8) 按 End 键回到行尾（物理列宽 6）
  (component-handle-input editor 'end)
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(6 36))
) ;let*

;; 4. 测试空格输入后硬件光标随文本位置移动，而不是回退到行尾
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (editor (pi-app-editor app))
      ) ;
  (pi-app-render! app)
  (pi-app-handle-input! app "hello")
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(5 36))

  (pi-app-handle-input! app " ")
  (pi-app-render! app)
  (check (editor-text editor) => "hello ")
  (check (terminal-cursor vterm) => '(6 36))

  (pi-app-handle-input! app "world")
  (pi-app-render! app)
  (check (editor-text editor) => "hello world")
  (check (terminal-cursor vterm) => '(11 36))

  ;; 行尾已有后续文本时，在单词之间插入空格也只应移动一列
  (pi-app-handle-input! app 'home)
  (pi-app-handle-input! app 'right)
  (pi-app-handle-input! app " ")
  (pi-app-render! app)
  (check (editor-text editor) => "h ello world")
  (check (terminal-cursor vterm) => '(2 36))
) ;let*

;; 5. 折行后行尾输入空格，光标位于第二显示行空格之后，而不是行首
(let* ((vterm (make-virtual-terminal :width 20 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (editor (pi-app-editor app))
      ) ;
  (pi-app-render! app)
  (pi-app-handle-input! app "aaaaaaaaaaaaaaaaaa bbbb") ; 18 个 a 折行，bbbb 位于第二显示行
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(4 36))

  (pi-app-handle-input! app " ")
  (pi-app-render! app)
  (check (editor-text editor) => "aaaaaaaaaaaaaaaaaa bbbb ")
  (check (terminal-cursor vterm) => '(5 36))
) ;let*

;; 6. 测试多轮对话与多行 Markdown 助手回复时，光标精准位于输入框行（不受 \n 干扰）
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
  ;; 其余为对话记录视口的空白行（视口 35 行）
  ;; Editor Top-Border: 行 35
  ;; Editor Content: 行 36
  ;; Editor Bottom-Border: 行 37
  ;; Footer: 行 38..39
  ;; 光标应位于输入行 (0, 36)，绝不能因 \n 漏算而向上漂移
  (check (terminal-cursor vterm) => '(0 36))

  ;; 第二轮输入文本 "测试第二轮"
  (component-handle-input editor "测试第二轮")
  (pi-app-render! app)
  ;; 5个汉字，物理列宽 10，行号仍为 16
  (check (terminal-cursor vterm) => '(10 36))

  ;; 按回车提交第二轮
  (component-handle-input editor 'enter)
  (pi-app-render! app)
  ;; 第二轮用户卡片追加 4 行 (行 15..18)，输入行平移至行 20
  (check (terminal-cursor vterm) => '(0 36))

  ;; 模拟第二轮助手完成回复
  (pi-app-add-assistant-text! app "第二轮回复：已完成测试")
  (pi-app-render! app)
  ;; 全屏布局：助手卡片追加到对话记录，输入行位置不变
  (check (terminal-cursor vterm) => '(0 36))

  ;; 第三轮输入 "第三轮验证"
  (component-handle-input editor "第三轮验证")
  (pi-app-render! app)
  ;; 5个中文字符，物理宽度 10，行号 36
  (check (terminal-cursor vterm) => '(10 36))

  ;; 按退格键删除 2 个字（剩 "第三轮"，物理宽度 6）
  (component-handle-input editor 'backspace)
  (component-handle-input editor 'backspace)
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(6 36))
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
  (let* ((lines (alt-screen-lines screen)) (plain-lines (map strip-ansi lines)))
    ;; Footer 第二行应包含透传的 provider 与 model（全屏布局下页脚位于底部）
    (check-true (string-contains? (list-ref plain-lines 39) "(kimi-coding) kimi-for-coding • medium"))
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
                (string-join (map strip-ansi (alt-screen-lines (pi-app-screen app))) "\n")
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
                  (string-join (map strip-ansi (alt-screen-lines (pi-app-screen app))) "\n")
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

(%dbg "[dbg7] ALL-DONE")

(check-report)
