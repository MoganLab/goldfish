(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive)
  (liii pi code modes interactive thinking)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

;; -----------------------------------------------------------------------------
;; cycle 切换 app 会话级 thinking level，同步 footer 标签与编辑器边框色
;; -----------------------------------------------------------------------------
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm
              :thinking-level "off" :supports-thinking #t))
       (ed (pi-app-editor app)))
  (check (pi-app-thinking-level app) => "off")
  (check (editor-border-color ed) => (pi-thinking-border-color "off"))
  (check (pi-app-cycle-thinking! app) => "minimal")
  (check (pi-app-thinking-level app) => "minimal")
  (check (editor-border-color ed) => (pi-thinking-border-color "minimal"))
  (pi-app-render! app)
  (check (string-contains?
           (string-join (screen-lines (pi-app-screen app)) "\n")
           "minimal") => #t))

;; 模型不支持 thinking：cycle 返回 #f 且状态不变
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)
             :thinking-level "off" :supports-thinking #f)))
  (check (pi-app-cycle-thinking! app) => #f)
  (check (pi-app-thinking-level app) => "off"))

;; 同步 handler 路径：runner 收到的 cfg 已被 app 当前 level 覆盖
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (received-cfg #f)
       (handler
         (make-tui-agent-handler
           (list (cons 'help #f))
           '((provider . "google") (api-key . "k")
             (thinking-level . "off") (model-id . "m"))
           (lambda (parsed cfg text emit finish)
             (set! received-cfg cfg)
             '())))
       (app (make-pi-app :terminal vterm :handler handler
              :thinking-level "high" :supports-thinking #t)))
  (pi-app-submit! app "hi")
  (check (cdr (assoc 'thinking-level received-cfg)) => "high"))

;; /thinking 打开选择器；方向键 + Enter 选择并恢复编辑器
(let* ((vterm (make-virtual-terminal :width 120 :height 50))
       (received '())
       (app (make-pi-app :terminal vterm
              :handler (lambda (a msg) (set! received (cons msg received)))
              :thinking-level "off" :default-thinking-level "off" :supports-thinking #t)))
  (pi-app-submit! app "/thinking")
  (check received => '())
  (check (editor-text (pi-app-editor app)) => "")
  (check-true (component? (pi-app-active-selector app)))
  (pi-app-render! app)
  (check-true
    (string-contains?
      (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")
      "Thinking Level"))
  (component-handle-input (pi-app-active-selector app) 'down)
  (component-handle-input (pi-app-active-selector app) 'enter)
  (check (pi-app-thinking-level app) => "minimal")
  (check (pi-app-active-selector app) => #f)
  (check (editor-border-color (pi-app-editor app)) => (pi-thinking-border-color "minimal"))
  (pi-app-render! app)
  (let ((text (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")))
    (check-true (string-contains? text "Thinking level: minimal"))
    (check-true (string-contains? text "gemini-3.8-flash • minimal")))
  (check received => '()))

;; Escape 取消选择器，不改变当前级别
(let ((app (make-pi-app :terminal (make-virtual-terminal 100 40)
             :handler (lambda (a m) #t) :thinking-level "medium" :supports-thinking #t)))
  (pi-app-submit! app "/thinking")
  (component-handle-input (pi-app-active-selector app) 'down)
  (component-handle-input (pi-app-active-selector app) 'escape)
  (check (pi-app-active-selector app) => #f)
  (check (pi-app-thinking-level app) => "medium"))

;; /thinking <level> 大小写不敏感直接设置；非法值显示可用级别
(let ((app (make-pi-app :terminal (make-virtual-terminal 120 40)
             :handler (lambda (a m) #t) :thinking-level "off" :supports-thinking #t)))
  (pi-app-submit! app "/thinking HIGH")
  (check (pi-app-thinking-level app) => "high")
  (check (pi-app-active-selector app) => #f)
  (pi-app-submit! app "/thinking bogus")
  (check (pi-app-thinking-level app) => "high")
  (pi-app-render! app)
  (check-true
    (string-contains?
      (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")
      "Unknown thinking level \"bogus\". Available levels: off, minimal, low, medium, high, xhigh, max.")))

;; Ctrl+S 更新当前级别、默认级别并调用持久化回调
(let* ((saved '())
       (app (make-pi-app :terminal (make-virtual-terminal 120 50)
              :handler (lambda (a m) #t) :thinking-level "off" :default-thinking-level "off"
              :supports-thinking #t
              :on-save-thinking-level (lambda (level) (set! saved (cons level saved))))))
  (pi-app-submit! app "/thinking")
  (component-handle-input (pi-app-active-selector app) 'down)
  (component-handle-input (pi-app-active-selector app) 'ctrl-s)
  (check saved => '("minimal"))
  (check (pi-app-thinking-level app) => "minimal")
  (check (pi-app-default-thinking-level app) => "minimal")
  (check (pi-app-active-selector app) => #f)
  (pi-app-render! app)
  (check-true
    (string-contains?
      (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")
      "Default thinking level: minimal")))

;; 不支持 thinking 的模型仅允许 off
(let ((app (make-pi-app :terminal (make-virtual-terminal 120 40)
             :handler (lambda (a m) #t) :thinking-level "off" :supports-thinking #f)))
  (pi-app-submit! app "/thinking high")
  (check (pi-app-thinking-level app) => "off")
  (pi-app-render! app)
  (check-true
    (string-contains?
      (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")
      "Available levels: off.")))

(check-report)
