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

(check-report)
