(import (liii check)
        (liii tui text-width)
        (liii tui component)
        (liii tui box))

(check-set-mode! 'report-failed)

;; make-hstack
;; 水平分列布局组件，将多个子组件横向并列排版

;; 1. 基础创建与 Component 协议检查
(let ((h (make-hstack (list "left" "right"))))
  (check (component? h) => #t)
  (check (hstack? h) => #t))

(let ((h (make-hstack "col1" "col2")))
  (check (component? h) => #t)
  (check (hstack? h) => #t))

;; 2. 默认均分宽度并排渲染
;; 总宽 20，两列各得 10 宽
(let* ((h (make-hstack "left" "right"))
       (lines (component-render h 20)))
  (check (length lines) => 1)
  (check (display-width (car lines)) => 20)
  (check (car lines) => "left      right     "))

;; 3. 列间距 spacing 支持
;; 总宽 20，两列，spacing=2：可用宽度 = 20 - 2 = 18，每列 9 宽
(let* ((h (make-hstack (list "left" "right") :spacing 2))
       (lines (component-render h 20)))
  (check (length lines) => 1)
  (check (display-width (car lines)) => 20)
  ;; "left" (4) + 5 空格 + 2 间距空格 + "right" (5) + 4 空格 = 20 宽
  (check (car lines) => "left       right    "))

;; 4. 自定义列宽 :widths
;; 明确指定固定列宽 '(5 15)
(let* ((h (make-hstack (list "col1" "col2") :widths '(5 15)))
       (lines (component-render h 20)))
  (check (length lines) => 1)
  (check (car lines) => "col1 col2           ")
  (check (display-width (car lines)) => 20))

;; 比例分配 :widths '(1 3) -> 1/4 与 3/4
(let* ((h (make-hstack (list "A" "B") :widths '(1 3)))
       (lines (component-render h 20)))
  ;; 20 * 1/4 = 5, 20 * 3/4 = 15
  (check (car lines) => "A    B              "))

;; 5. 多行与不同高度垂直对齐 (valign)
(let* ((c1 (make-text "1\n2\n3"))
       (c2 (make-text "only-one"))
       ;; 默认顶部对齐 'top
       (h-top (make-hstack (list c1 c2) :widths '(6 10)))
       (lines-top (component-render h-top 16)))
  (check (length lines-top) => 3)
  (check (list-ref lines-top 0) => "1     only-one  ")
  (check (list-ref lines-top 1) => "2               ")
  (check (list-ref lines-top 2) => "3               "))

;; 底部对齐 'bottom
(let* ((c1 (make-text "1\n2\n3"))
       (c2 (make-text "only-one"))
       (h-bot (make-hstack (list c1 c2) :widths '(6 10) :valign 'bottom))
       (lines-bot (component-render h-bot 16)))
  (check (length lines-bot) => 3)
  (check (list-ref lines-bot 0) => "1               ")
  (check (list-ref lines-bot 1) => "2               ")
  (check (list-ref lines-bot 2) => "3     only-one  "))

;; 6. CJK 与 ANSI 显示宽度严格对齐
;; 左列包含汉字 "中文" (视觉宽度 4)，右列包含 ANSI 颜色文本
(let* ((c1 "中文")
       (c2 "\x1b[32mOK\x1b[0m")
       (h (make-hstack (list c1 c2) :widths '(10 10)))
       (lines (component-render h 20)))
  (check (length lines) => 1)
  (check (display-width (car lines)) => 20))

;; 7. 事件级联与状态刷新
(let* ((handled #f)
       (c1 (make-component :render (lambda (w) '("c1"))))
       (c2 (make-component
             :render (lambda (w) '("c2"))
             :handle-input (lambda (k) (set! handled k) #t)))
       (h (make-hstack (list c1 c2))))
  (check (component-handle-input h 'tab) => #t)
  (check handled => 'tab))

;; 8. 边界与异常
(let ((h-empty (make-hstack '())))
  (check (component-render h-empty 20) => '()))

(check-catch 'type-error (make-hstack (list 123)))
(check-catch 'value-error (make-hstack (list "a" "b") :widths '(10)))
(check-catch 'value-error (make-hstack "a" "b" :spacing -1))

(check-report)
