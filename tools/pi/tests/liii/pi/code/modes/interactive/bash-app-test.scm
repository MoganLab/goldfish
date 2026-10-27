(import (scheme base)
  (liii check)
  (liii string)
  (liii list)
  (liii time)
  (liii unicode)
  (liii go)
  (liii tui)
  (liii pi code modes interactive bash)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report)

;; 构造 1..n 的多行输出文本
(define (%nums-text n)
  (string-join (map number->string (iota n 1)) "\n")
) ;define

;; ============================================================
;; 二、App 集成：bash mode 边框变色与提交执行
;; ============================================================

;; 8. 输入 "!" 后编辑器边框变绿，删除后恢复 thinking 色
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vt
              :handler      (lambda (a m) #t)
              ;; 注入同步 bash 执行器，避免测试进程派生后台 go 线程
              :bash-handler (lambda (cmd) (values "" 0))
            ) ;make-pi-app
           )
       (ed (pi-app-editor app))
       (screen (pi-app-screen app))
      ) ;
  (pi-app-render! app)
  ;; 初始边框为 thinking off 灰色，不含 bash 绿
  (check-false (string-contains? (list-ref (screen-lines screen) 6) "38;2;94;178;134"))
  ;; 输入 !ls → 边框变绿
  (component-handle-input ed "!")
  (pi-app-render! app)
  (check-true (string-contains? (list-ref (screen-lines screen) 6) "38;2;94;178;134"))
  ;; 前导空格 + ! 同样进入 bash mode（对齐 pi 的 trimStart 语义）
  (editor-set-text! ed "  !ls")
  (pi-app-render! app)
  (check-true (string-contains? (list-ref (screen-lines screen) 6) "38;2;94;178;134"))
  ;; 删除全部字符 → 恢复 thinking 色
  (editor-set-text! ed "")
  (pi-app-render! app)
  (check-false (string-contains? (list-ref (screen-lines screen) 6) "38;2;94;178;134"))
) ;let*

;; 9. 提交 !cmd：执行 bash 卡片入 chat，不进消息队列，边框恢复
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (agent-called '())
       (dummy-agent (lambda (a m) (set! agent-called (cons m agent-called))))
       (bash-exec (lambda (cmd) (values (string-append "out-of:" cmd) 0)))
       (app (make-pi-app :terminal vt :handler dummy-agent :bash-handler bash-exec))
       (ed (pi-app-editor app))
       (screen (pi-app-screen app))
      ) ;
  (pi-app-render! app)
  (component-handle-input ed "!echo hi")
  (component-handle-input ed 'enter)
  (pi-app-render! app)
  (let ((lines (screen-lines screen)))
    ;; bash 卡片已上屏：卡片首行为绿色边框，命令头与输出紧随
    (check-true (string-contains? (list-ref lines 6) "38;2;94;178;134"))
    (check-true (string-contains? (list-ref lines 7) "$ echo hi"))
    (check-true (string-contains? (list-ref lines 9) "out-of:echo hi"))
    ;; 编辑器清空且边框恢复非绿色（卡片 6 行后编辑器上边框在索引 12）
    (check (editor-text ed) => "")
    (check-false (string-contains? (list-ref lines 12) "38;2;94;178;134"))
  ) ;let
  ;; agent handler 未被调用：bash 命令不发给模型
  (check agent-called => '())
) ;let*

;; 10. 单个 "!" 提交：命令为空，回退为普通消息提交（对齐 pi fall-through）
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (agent-called '())
       (dummy-agent (lambda (a m) (set! agent-called (cons m agent-called))))
       (bash-exec (lambda (cmd) (values "should-not-run" 0)))
       (app (make-pi-app :terminal vt :handler dummy-agent :bash-handler bash-exec))
       (ed (pi-app-editor app))
       (screen (pi-app-screen app))
      ) ;
  (pi-app-render! app)
  (component-handle-input ed "!")
  (component-handle-input ed 'enter)
  (pi-app-render! app)
  ;; 不执行 bash，而是作为普通消息进入 agent 队列
  (check agent-called => '("!"))
  (check (editor-text ed) => "")
  (check-true (not (string-contains? (string-join (screen-lines screen) "\n") "$ !")))
) ;let*

;; 11. Ctrl+O 全局切换：折叠卡片展开显示全部行，再按恢复折叠
(let* ((vt (make-virtual-terminal :width 100 :height 60))
       (bash-exec (lambda (cmd) (values (%nums-text 35) 0)))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t) :bash-handler bash-exec))
       (ed (pi-app-editor app))
       (screen (pi-app-screen app))
      ) ;
  (pi-app-render! app)
  (component-handle-input ed "!seq 1 35")
  (component-handle-input ed 'enter)
  (pi-app-render! app)
  ;; 折叠态：首输出行为 16，含展开提示
  (let ((joined (string-join (map strip-ansi (screen-lines screen)) "\n")))
    (check-true (string-contains? joined " 16"))
    (check-false (string-contains? joined " 1\n"))
    (check-true (string-contains? joined "... 15 more lines (ctrl+o to expand)"))
  ) ;let
  ;; Ctrl+O 切换前诊断：chat 内应有一张 bash 卡片组件
  (check (length (pi-app-chat app)) => 1)
  (check-true (bash-card-component? (car (pi-app-chat app))))
  ;; Ctrl+O 切换 → 展开
  (check (pi-app-toggle-tool-output! app) => #t)
  (check-true (bash-card-expanded? (car (pi-app-chat app))))
  (pi-app-render! app)
  (let ((joined (string-join (map strip-ansi (screen-lines screen)) "\n")))
    (check-true (string-contains? joined " 1\n"))
    (check-true (string-contains? joined " 35\n"))
    (check-true (string-contains? joined "(ctrl+o to collapse)"))
  ) ;let
  ;; 再次 Ctrl+O → 恢复折叠
  (check (pi-app-toggle-tool-output! app) => #f)
  (pi-app-render! app)
  (let ((joined (string-join (map strip-ansi (screen-lines screen)) "\n")))
    (check-true (string-contains? joined "... 15 more lines (ctrl+o to expand)"))
  ) ;let
) ;let*

;; ============================================================
;; 三、上下文注入（对齐 pi recordBashResult）：
;; !cmd 结果经 record-sink 注入会话上下文；!!cmd 仅显示不注入
;; ============================================================

;; 13. !cmd：同步 bash-handler 路径完成后调用 record-sink（命令、输出、退出码）
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (recorded '())
       (app (make-pi-app :terminal vt
              :handler      (lambda (a m) #t)
              :bash-handler (lambda (cmd) (values "out-13" 0))
              :record-sink  (lambda (command text code)
                              (set! recorded (cons (list command text code) recorded)))
            ) ;make-pi-app
           )
       (ed (pi-app-editor app))
      ) ;
  (component-handle-input ed "!echo hi")
  (component-handle-input ed 'enter)
  (check recorded => '(("echo hi" "out-13" 0)))
) ;let*

;; 14. !!cmd：命令执行、卡片上屏（暗色边框），但 record-sink 不被调用
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (recorded '())
       (bash-calls '())
       (app (make-pi-app :terminal vt
              :handler      (lambda (a m) #t)
              :bash-handler (lambda (cmd)
                              (set! bash-calls (cons cmd bash-calls))
                              (values "hidden" 0))
              :record-sink  (lambda (command text code)
                              (set! recorded (cons command recorded)))
            ) ;make-pi-app
           )
       (ed (pi-app-editor app))
       (screen (pi-app-screen app))
      ) ;
  (pi-app-render! app)
  (component-handle-input ed "!!whoami")
  (component-handle-input ed 'enter)
  (pi-app-render! app)
  ;; 命令仍执行且卡片上屏
  (check bash-calls => '("whoami"))
  (let ((joined (string-join (screen-lines screen) "\n")))
    (check-true (string-contains? joined "$ whoami"))
    (check-true (string-contains? joined "hidden"))
    ;; 排除卡片用暗色边框而非 bash 绿色（全屏无 bash 绿）
    (check-false (string-contains? joined "38;2;94;178;134"))
  ) ;let
  ;; 不注入上下文
  (check recorded => '())
) ;let*

;; 15. 单个 "!!"：命令为空，回退为普通消息提交（对齐 pi fall-through）
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (agent-called '())
       (app (make-pi-app :terminal vt
              :handler      (lambda (a m) (set! agent-called (cons m agent-called)))
              :bash-handler (lambda (cmd) (values "should-not-run" 0))
              :record-sink  (lambda args (error 'sink "must not be called"))
            ) ;make-pi-app
           )
       (ed (pi-app-editor app))
      ) ;
  (component-handle-input ed "!!")
  (component-handle-input ed 'enter)
  (check agent-called => '("!!"))
) ;let*

;; 16. 默认 record-sink：bash 完成后投递 (bash_record 命令 输出 退出码) 到 jobs-chan
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vt
              :handler      (lambda (a m) #t)
              :bash-handler (lambda (cmd) (values "out-16" 0))
            ) ;make-pi-app
           )
       (ed (pi-app-editor app))
      ) ;
  (component-handle-input ed "!echo hi")
  (component-handle-input ed 'enter)
  (check (chan-try-recv! (pi-app-jobs-chan app) #f) => '(bash_record "echo hi" "out-16" 0))
) ;let*

;; 17. 生产路径（无 bash-handler）：经后台 pi-bash-worker 协程执行并回填卡片。
;; 复现曾出现的卡片永远停在 Running...：worker 与事件轮询链路必须闭环
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vt
              ;; 只设 agent handler（阻止 agent worker 派生），
              ;; 不设 bash-handler → 启动真实后台 bash worker
              :handler (lambda (a m) #t)
            ) ;make-pi-app
           )
       (bash-card #f)
      ) ;
  (pi-app-run-bash! app "echo worker-ok")
  (set! bash-card (cdr (car (%pi-app-bash-cards app))))
  ;; 轮询事件直至完成：以“不再显示 Running...”为准（命令文本本身含 worker-ok，
  ;; 不能作为完成信号），最长 ~3s
  (let loop ((i 0))
    (pi-app-poll-events! app)
    (let ((text (string-join (map strip-ansi (component-render bash-card 80)) "\n")))
      (unless (or (not (string-contains? text "Running...")) (>= i 100))
        (sleep 0.03)
        (loop (+ i 1))
      ) ;unless
    ) ;let
  ) ;let
  (let ((text (string-join (map strip-ansi (component-render bash-card 80)) "\n")))
    (check-true (string-contains? text "worker-ok"))
    ;; 已结束：不再显示 Running...
    (check-false (string-contains? text "Running..."))
  ) ;let
  ;; 结果注入 record-sink：jobs-chan 收到 (bash_record "echo worker-ok" ... 0)
  (let ((ev (chan-try-recv! (pi-app-jobs-chan app) #f)))
    (check (car ev) => 'bash_record)
    (check (cadr ev) => "echo worker-ok")
    (check-true (string-contains? (list-ref ev 2) "worker-ok"))
    (check (list-ref ev 3) => 0)
  ) ;let
) ;let*

(check-report)
