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

(check-report)
