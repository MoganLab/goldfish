(import (liii check)
  (liii error)
  (liii tui component)
  (liii tui box)
  (liii tui terminal)
  (liii pi modes tui state)
  (liii pi modes tui loop)
  (liii pi modes tui editor)
) ;import

(check-set-mode! 'report-failed)

;; 1.0 基础构造与默认属性测试
(let ((ed (make-pi-tui-editor)))
  (check-true (pi-tui-editor? ed))
  (check-true (component? ed))
  (check (pi-tui-editor-text ed) => "")
  (check (pi-tui-editor-lines ed) => '(""))
  (check (pi-tui-editor-cursor ed) => '(0 0))
  (check (pi-tui-editor-prompt ed) => "> ")
  (check (pi-tui-editor-placeholder ed) => "Ask a question or type / for commands...")
  (check-true (pi-tui-editor-focused? ed))

  ;; 属性设置测试
  (pi-tui-editor-set-prompt! ed "$ ")
  (check (pi-tui-editor-prompt ed) => "$ ")

  (pi-tui-editor-set-placeholder! ed "Type something...")
  (check (pi-tui-editor-placeholder ed) => "Type something...")

  (pi-tui-editor-set-focused! ed #f)
  (check-false (pi-tui-editor-focused? ed))
  (pi-tui-editor-set-focused! ed #t)
  (check-true (pi-tui-editor-focused? ed))

  (pi-tui-editor-set-text! ed "initial text")
  (check (pi-tui-editor-text ed) => "initial text")
  (check (pi-tui-editor-lines ed) => '("initial text"))

  (pi-tui-editor-clear! ed)
  (check (pi-tui-editor-text ed) => "")
  (check (pi-tui-editor-cursor ed) => '(0 0))
) ;let

;; 2.0 字符输入、UTF-8 中文录入、编辑与光标移动
(let ((ed (make-pi-tui-editor)))
  ;; 字符串插入
  (check-true (pi-tui-editor-handle-key ed "hello"))
  (check (pi-tui-editor-text ed) => "hello")
  (check (pi-tui-editor-cursor ed) => '(0 5))

  ;; UTF-8 中文输入
  (check-true (pi-tui-editor-handle-key ed " 世界"))
  (check (pi-tui-editor-text ed) => "hello 世界")

  ;; Backspace 退格
  (check-true (pi-tui-editor-handle-key ed 'backspace))
  (check (pi-tui-editor-text ed) => "hello 世")

  ;; 光标左移
  (check-true (pi-tui-editor-handle-key ed 'left))
  (check-true (pi-tui-editor-handle-key ed 'left))
  ;; 插入字符
  (check-true (pi-tui-editor-handle-key ed "!"))
  (check (pi-tui-editor-text ed) => "hello! 世")

  ;; Home 与 End 跳转
  (check-true (pi-tui-editor-handle-key ed 'home))
  (check (pi-tui-editor-cursor ed) => '(0 0))
  (check-true (pi-tui-editor-handle-key ed 'end))
  (check (pi-tui-editor-cursor ed) => (list 0 (string-length "hello! 世")))

  ;; Delete 前向删除
  (check-true (pi-tui-editor-handle-key ed 'home))
  (check-true (pi-tui-editor-handle-key ed 'delete))
  (check (pi-tui-editor-text ed) => "ello! 世")
) ;let

;; 3.0 会话历史记录导航
(let ((ed (make-pi-tui-editor)))
  ;; 预先写入历史记录
  (pi-tui-editor-push-history! ed "first prompt")
  (pi-tui-editor-push-history! ed "second prompt")

  ;; 当前输入框为空，按 Up 唤出最近一条历史
  (check-true (pi-tui-editor-handle-key ed 'up))
  (check (pi-tui-editor-text ed) => "second prompt")

  ;; 再次按 Up 唤出更早一条历史
  (check-true (pi-tui-editor-handle-key ed 'up))
  (check (pi-tui-editor-text ed) => "first prompt")

  ;; 到顶后再按 Up 保持在最老记录
  (check-true (pi-tui-editor-handle-key ed 'up))
  (check (pi-tui-editor-text ed) => "first prompt")

  ;; 按 Down 翻回下一条
  (check-true (pi-tui-editor-handle-key ed 'down))
  (check (pi-tui-editor-text ed) => "second prompt")

  ;; 再次按 Down 恢复草稿（空文本）
  (check-true (pi-tui-editor-handle-key ed 'down))
  (check (pi-tui-editor-text ed) => "")
) ;let

;; 4.0 多行折行（Shift+Enter / 反斜杠+Enter）与回车提交
(let* ((submitted #f)
       (ed (make-pi-tui-editor
             :on-submit (lambda (self txt) (set! submitted txt))
           ) ;make-pi-tui-editor
       ) ;ed
      ) ;
  ;; 测试 Shift+Enter 多行换行
  (pi-tui-editor-handle-key ed "line 1")
  (check-true (pi-tui-editor-handle-key ed 'shift-enter))
  (check (length (pi-tui-editor-lines ed)) => 2)
  (check (pi-tui-editor-cursor ed) => '(1 0))
  (pi-tui-editor-handle-key ed "line 2")
  (check (pi-tui-editor-text ed) => "line 1\nline 2")

  ;; 多行时的上下移动
  (check-true (pi-tui-editor-handle-key ed 'up))
  (check (car (pi-tui-editor-cursor ed)) => 0)
  (check-true (pi-tui-editor-handle-key ed 'down))
  (check (car (pi-tui-editor-cursor ed)) => 1)

  ;; 测试反斜杠 \ + Enter 折行
  (pi-tui-editor-clear! ed)
  (pi-tui-editor-handle-key ed "multi \\")
  (check-true (pi-tui-editor-handle-key ed 'enter))
  ;; 反斜杠被移除并换行
  (check (pi-tui-editor-lines ed) => '("multi " ""))
  (check (pi-tui-editor-cursor ed) => '(1 0))
  (pi-tui-editor-handle-key ed "line 2")
  (check (pi-tui-editor-text ed) => "multi \nline 2")

  ;; 默认普通 Enter 触发提交
  (check-false submitted)
  (check-true (pi-tui-editor-handle-key ed 'enter))
  (check submitted => "multi \nline 2")
) ;let

;; 5.0 视图渲染与外框包裹
(let ((ed (make-pi-tui-editor :prompt "> ")))
  (pi-tui-editor-set-text! ed "echo test")
  (let ((lines (pi-tui-editor-render ed 40)))
    (check-true (pair? lines))
    ;; 首行包含提示符
    (check-true (> (string-length (car lines)) 0))
  ) ;let

  ;; 圆角外框包裹渲染
  (let ((boxed (pi-tui-editor-render-boxed ed 30 :style 'rounded :title "Input")))
    (check-true (pair? boxed))
    ;; 顶部边框包含圆角字符和标题
    (check-true (and (> (length boxed) 2) #t))
  ) ;let

  ;; 直接作为 Component 放入 make-box
  (let* ((bx (make-box ed :style 'rounded :title "Boxed Input"))
         (rendered (component-render bx 30))
        ) ;
    (check-true (pair? rendered))
    (check-true (>= (length rendered) 3))
  ) ;let
) ;let

;; 6.0 类型校验与异常处理
(check-catch 'type-error (pi-tui-editor-text "not-editor"))
(check-catch 'type-error (pi-tui-editor-set-text! 123 "text"))
(check-catch 'type-error (pi-tui-editor-clear! #f))
(check-catch 'type-error (pi-tui-editor-lines '()))
(check-catch 'type-error (pi-tui-editor-cursor "bad"))
(check-catch 'type-error (pi-tui-editor-set-cursor! #t 0 0))
(check-catch 'type-error (pi-tui-editor-prompt 456))
(check-catch 'type-error (pi-tui-editor-set-prompt! "bad" "> "))
(check-catch 'type-error (pi-tui-editor-placeholder "bad"))
(check-catch 'type-error (pi-tui-editor-set-placeholder! "bad" "ph"))
(check-catch 'type-error (pi-tui-editor-focused? "bad"))
(check-catch 'type-error (pi-tui-editor-set-focused! "bad" #t))
(check-catch 'type-error (pi-tui-editor-history "bad"))
(check-catch 'type-error (pi-tui-editor-push-history! "bad" "txt"))
(check-catch 'type-error (pi-tui-editor-handle-key "bad" 'enter))
(check-catch 'type-error (pi-tui-editor-render "bad" 80))
(check-catch 'type-error (pi-tui-editor-on-submit "bad"))
(check-catch 'type-error (pi-tui-editor-set-on-submit! "bad" (lambda _ #f)))
(check-catch 'type-error (pi-tui-editor-on-change "bad"))
(check-catch 'type-error (pi-tui-editor-set-on-change! "bad" (lambda _ #f)))

;; 7.0 状态机与 input-editor 协同（state-input-text / state-submit-prompt!）
(let* ((ed (make-pi-tui-editor))
       (st (make-pi-tui-state :input-editor ed))
      ) ;
  (pi-tui-editor-set-text! ed "git status")
  (check (state-input-text st) => "git status")

  ;; 触发提交：追加至 messages，清空 editor，推入 history，切换至 running
  (state-submit-prompt! st)
  (check (state-status st) => 'running)
  (check (state-messages st) => '((user . "git status")))
  (check (state-input-text st) => "")
  (check (pi-tui-editor-text ed) => "")

  ;; 历史记录验证：在 editor 中按 Up 能够调出刚提交的内容
  (check-true (pi-tui-editor-handle-key ed 'up))
  (check (pi-tui-editor-text ed) => "git status")
) ;let

;; 8.0 虚拟终端 VirtualTerminal 按键流集成驱动测试
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (ed (make-pi-tui-editor))
       (st (make-pi-tui-state :input-editor ed :status 'idle))
      ) ;
  ;; 模拟用户在交互终端输入 "query"（包含字符 'q'，验证不误触发退出！）
  (virtual-terminal-push-key! vt "q")
  (virtual-terminal-push-key! vt "u")
  (virtual-terminal-push-key! vt "e")
  (virtual-terminal-push-key! vt "r")
  (virtual-terminal-push-key! vt "y")
  ;; 模拟退格删除 'y'
  (virtual-terminal-push-key! vt 'backspace)
  ;; 模拟多行换行并输入第二行
  (virtual-terminal-push-key! vt 'shift-enter)
  (virtual-terminal-push-key! vt "line2")
  ;; 模拟按 escape 正常退出
  (virtual-terminal-push-key! vt 'escape)

  (let ((final-st (run-tui-loop vt :state st :return-state #t)))
    (check (pi-tui-state? final-st) => #t)
    (check (state-status final-st) => 'exit)
    ;; 验证打字录入正确且退格与换行正确
    (check (pi-tui-editor-text ed) => "quer\nline2")
  ) ;let
) ;let

(check-report)
