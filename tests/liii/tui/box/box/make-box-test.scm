(import (liii check)
        (liii tui text-width)
        (liii tui component)
        (liii tui box))

(check-set-mode! 'report-failed)

;; make-box
;; 边框容器组件，支持多种边框风格、内边距与标题配置

;; 1. 基础创建与 Component 协议检查
(let ((b (make-box "hello" :style 'ascii)))
  (check (component? b) => #t)
  (check (box? b) => #t))

;; 2. ASCII 边框渲染
;; width = 9, 默认 padding 0:
;; +-------+
;; |hello  |
;; +-------+
(let* ((b (make-box "hello" :style 'ascii))
       (lines (component-render b 9)))
  (check (length lines) => 3)
  (check (car lines) => "+-------+")
  (check (cadr lines) => "|hello  |")
  (check (caddr lines) => "+-------+"))

;; 3. Unicode 单线与双线边框
(let* ((b (make-box "test" :style 'single))
       (lines (component-render b 8)))
  (check (length lines) => 3)
  (check (display-width (car lines)) => 8)
  (check (car lines) => "┌──────┐")
  (check (cadr lines) => "│test  │")
  (check (caddr lines) => "└──────┘"))

(let* ((b (make-box "ab" :style 'double))
       (lines (component-render b 6)))
  (check (car lines) => "╔════╗")
  (check (cadr lines) => "║ab  ║")
  (check (caddr lines) => "╚════╝"))

;; 4. 圆角边框与标题
(let* ((b (make-box "content" :style 'rounded :title "Info"))
       (lines (component-render b 14)))
  (check (display-width (car lines)) => 14)
  (check (car lines) => "╭─ Info ─────╮")
  (check (display-width (cadr lines)) => 14)
  (check (cadr lines) => "│content     │")
  (check (caddr lines) => "╰────────────╯"))

;; 5. 内边距 padding 支持
;; :padding 1 -> 上下各 1 行空白，左右各 1 列缩进
(let* ((b (make-box "hi" :style 'ascii :padding 1))
       (lines (component-render b 8)))
  ;; width = 8, overhead = 2(border) + 2(padding) = 4, content-w = 4
  ;; "hi" 在 4 列中左对齐填充为 "hi  "
  ;; +------+
  ;; |      |
  ;; | hi   |
  ;; |      |
  ;; +------+
  (check (length lines) => 5)
  (check (car lines) => "+------+")
  (check (list-ref lines 1) => "|      |")
  (check (list-ref lines 2) => "| hi   |")
  (check (list-ref lines 3) => "|      |")
  (check (list-ref lines 4) => "+------+"))

;; 细粒度 padding 支持
(let* ((b (make-box "hi" :style 'ascii :padding-left 2 :padding-top 1))
       (lines (component-render b 8)))
  (check (length lines) => 4)
  (check (car lines) => "+------+")
  (check (list-ref lines 1) => "|      |")
  (check (list-ref lines 2) => "|  hi  |")
  (check (list-ref lines 3) => "+------+"))

;; 多行内容与上下内边距保持原有顺序。
(let* ((b (make-box "one\ntwo" :style 'ascii :padding-top 2 :padding-bottom 1))
       (lines (component-render b 7)))
  (check lines =>
    '("+-----+" "|     |" "|     |" "|one  |" "|two  |" "|     |" "+-----+")))

;; 6. 'none 风格边框（无边框，仅内边距 padding 效果）
(let* ((b (make-box "text" :style 'none :padding-left 2))
       (lines (component-render b 10)))
  (check (length lines) => 1)
  (check (car lines) => "  text    ")
  (check (display-width (car lines)) => 10))

(let* ((b (make-box "one\ntwo" :style 'none :padding-top 1 :padding-bottom 1))
       (lines (component-render b 4)))
  (check lines => '("    " "one " "two " "    ")))

;; 7. 自动折行支持与宽度收缩自适应
;; 内部 child 为长文本，当 width 变小时自动在可用内容宽度内折行
(let* ((b (make-box "hello world foo bar" :style 'ascii))
       (lines (component-render b 10)))
  ;; 可用内容宽度 = 10 - 2 = 8
  ;; "hello world foo bar" 在 8 列内折行: "hello", "world", "foo bar"
  (check (> (length lines) 3) => #t)
  (for-each
    (lambda (line)
      (check (display-width line) => 10))
    lines))

;; 8. 极限边界安全保护（宽度过小，不崩溃）
(let ((b (make-box "hello" :style 'ascii)))
  (check (component-render b 0) => '())
  (let ((l1 (component-render b 1)))
    (check (length l1) => 3)
    (check (car l1) => "+"))
  (let ((l2 (component-render b 2)))
    (check (length l2) => 3)
    (check (car l2) => "++")))

;; 9. 事件与状态下发
(let* ((handled-key #f)
       (invalidated #f)
       (child (make-component
                :render (lambda (w) (list "inner"))
                :handle-input (lambda (k) (set! handled-key k) #t)
                :invalidate (lambda () (set! invalidated #t) #t)))
       (b (make-box child :style 'single)))
  (check (component-handle-input b 'up) => #t)
  (check handled-key => 'up)
  (check (component-invalidate! b) => #t)
  (check invalidated => #t))

;; 10. 非法参数类型拦截
(check-catch 'type-error (make-box 12345))
(check-catch 'type-error (make-box "hi" :padding "1"))
(check-catch 'value-error (make-box "hi" :padding -1))

(check-report)
