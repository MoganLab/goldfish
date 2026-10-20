(import (liii check)
        (liii tui))

(check-set-mode! 'report-failed)

;; (liii tui) - 终端用户界面 (TUI) 通用库
;; 提供终端 UI 组件核心协议、基础文本与占位组件。

;; 构造函数
;; make-component - 创建自定义组件
;; make-text - 创建按宽度自动折行的文本组件
;; make-truncated-text - 创建单行截断文本组件
;; make-spacer - 创建空白占位组件

;; 谓词
;; component? - 检查对象是否为 TUI 组件

;; 操作
;; component-render - 渲染组件为行列表
;; component-handle-input - 分发键盘或事件输入
;; component-invalidate! - 标记组件脏区或刷新

;; 文本宽度工具
;; strip-ansi - 剔除 ANSI 控制字符
;; display-width - 计算视觉显示宽度
;; truncate-to-width - 安全截断文本
;; wrap-text - 按显示宽度安全折行

;; 键盘输入解析工具
;; parse-key - 解析单个按键
;; parse-keys - 批量解析按键输入流

(let ((c (make-text "hello world")))
  (check (component? c) => #t)
  (check (component-render c 5) => '("hello" "world"))
  (check (component-handle-input c 'up) => #f)
  (check (component-invalidate! c) => #t))

(let ((c (make-truncated-text "hello world")))
  (check (component? c) => #t)
  (check (component-render c 8) => '("hello...")))

(let ((c (make-spacer 2)))
  (check (component? c) => #t)
  (check (component-render c 40) => '("" "")))

(check (strip-ansi "\x1b[31mhi\x1b[0m") => "hi")
(check (display-width "金鱼") => 4)
(check (truncate-to-width "你好世界" 7) => "你好...")
(check (wrap-text "你好世界" 4) => '("你好" "世界"))

(check (parse-key "\x1b[A") => 'up)
(check (parse-key "\r\n") => 'enter)
(check (parse-key "\x03") => 'ctrl-c)
(check (parse-keys "w\x1b[As\x1b[B") => '("w" up "s" down))
(check (parse-keys "金鱼") => '("金" "鱼"))

;; 终端抽象测试
(let ((vt (make-virtual-terminal :width 20 :height 5)))
  (check (terminal? vt) => #t)
  (check (virtual-terminal? vt) => #t)
  (check (terminal-size vt) => '(20 5))
  (terminal-write vt "Goldfish TUI")
  (check (car (virtual-terminal-screen vt)) => "Goldfish TUI        ")
  (terminal-clear vt)
  (check (terminal-cursor vt) => '(0 0)))

(let* ((p (open-output-string))
       (st (make-standard-terminal :output-port p)))
  (check (terminal? st) => #t)
  (check (standard-terminal? st) => #t)
  (terminal-write st "hi")
  (check (get-output-string p) => "hi"))

;; 布局容器组件测试
(let ((b (make-box "content" :style 'single :title "Box")))
  (check (component? b) => #t)
  (check (box? b) => #t)
  (check (display-width (car (component-render b 16))) => 16))

(let ((v (make-vstack "row 1" "row 2" :spacing 1)))
  (check (component? v) => #t)
  (check (vstack? v) => #t)
  (check (length (component-render v 20)) => 3))

(let ((h (make-hstack "col 1" "col 2" :spacing 2)))
  (check (component? h) => #t)
  (check (hstack? h) => #t)
  (check (display-width (car (component-render h 20))) => 20))

(check (border-style? (border-style-resolve 'rounded)) => #t)

;; 转轮动画与状态组件测试
(let ((s (make-spinner "Loading..." :frames 'line)))
  (check (component? s) => #t)
  (check (spinner? s) => #t)
  (check (loader? s) => #t)
  (check (component-render s 40) => '("- Loading..."))
  (check (spinner-tick! s) => "\\")
  (check (component-render s 40) => '("\\ Loading..."))
  (spinner-success! s "Done")
  (check (spinner-status s) => 'success)
  (check (component-render s 40) => '("✔ Done")))

(let ((l (make-loader :frames 'dots :visible #f)))
  (check (loader? l) => #t)
  (check (spinner-visible? l) => #f)
  (check (component-render l 40) => '())
  (spinner-show! l)
  (check (spinner-visible? l) => #t))

(check (pair? (spinner-presets)) => #t)
(check (list? (spinner-frames-resolve 'pipe)) => #t)

;; 编辑器组件门面再导出测试
(let ((ed (make-editor "hello" :cursor-style 'bar)))
  (check (component? ed) => #t)
  (check (editor? ed) => #t)
  (check (editor-text ed) => "hello")
  (check (editor-cursor ed) => '(0 5))
  (component-handle-input ed 'left)
  (check (editor-cursor ed) => '(0 4))
  (check (component-render ed 20) => '("hell|o")))

(let ((ti (make-text-input "cmd" :placeholder "prompt")))
  (check (text-input? ti) => #t)
  (check (editor-placeholder ti) => "prompt")
  (component-handle-input ti 'enter)
  (check (editor-history-list ti) => '("cmd")))

(let ((ta (make-text-area "multi\nline")))
  (check (text-area? ta) => #t)
  (check (editor-lines ta) => '("multi" "line")))

;; 主屏幕驱动与差分渲染门面再导出测试
(let* ((vt (make-virtual-terminal :width 16 :height 3))
       (screen (make-main-screen vt)))
  (check (main-screen? screen) => #t)
  (check (screen-render! screen '("Hello TUI")) => #t)
  (check (screen-lines screen) => '("Hello TUI"))
  (check (car (virtual-terminal-screen vt)) => "Hello TUI       ")
  (check (pair? (screen-diff '("a") '("b"))) => #t)
  (screen-clear! screen)
  (check (screen-lines screen) => '()))

(check-report)
