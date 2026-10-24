(import (scheme base)
  (liii check)
  (liii unicode)
  (liii string)
  (liii tui)
  (liii pi code tui app)
) ;import

(check-set-mode! 'report)

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
    ;; 追加了用户卡片与助手回复卡片（各占内容行与间隔空行）
    (check (length lines) => 15)
    (check-true (string-contains? (list-ref lines 6) "ping"))
    (check-true (string-contains? (list-ref lines 8) "处理完成: ping"))
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
  ;; User Card: 2行 (6..7)
  ;; Assistant Card: 4行内容 + 1空行 = 5行 (8..12)
  ;; Editor Top-Border: 行 13
  ;; Editor Content: 行 14
  ;; Editor Bottom-Border: 行 15
  ;; Footer: 行 16..17
  ;; 光标应位于输入行 (0, 14)，绝不能因 \n 漏算而向上漂移
  (check (terminal-cursor vterm) => '(0 14))

  ;; 第二轮输入文本 "测试第二轮"
  (component-handle-input editor "测试第二轮")
  (pi-app-render! app)
  ;; 5个汉字，物理列宽 10，行号仍为 14
  (check (terminal-cursor vterm) => '(10 14))

  ;; 按回车提交第二轮
  (component-handle-input editor 'enter)
  (pi-app-render! app)
  ;; 第二轮用户卡片追加 2 行 (行 13..14)，输入行平移至行 16
  (check (terminal-cursor vterm) => '(0 16))

  ;; 模拟第二轮助手完成回复
  (pi-app-add-assistant-text! app "第二轮回复：已完成测试")
  (pi-app-render! app)
  ;; Assistant 2 追加 2 行 (行 15..16)，输入行平移至行 18
  (check (terminal-cursor vterm) => '(0 18))

  ;; 第三轮输入 "第三轮验证"
  (component-handle-input editor "第三轮验证")
  (pi-app-render! app)
  ;; 5个中文字符，物理宽度 10，行号 18
  (check (terminal-cursor vterm) => '(10 18))

  ;; 按退格键删除 2 个字（剩 "第三轮"，物理宽度 6）
  (component-handle-input editor 'backspace)
  (component-handle-input editor 'backspace)
  (pi-app-render! app)
  (check (terminal-cursor vterm) => '(6 18))
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

(check-report)
