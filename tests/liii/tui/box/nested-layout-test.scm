(import (liii check)
        (liii tui text-width)
        (liii tui component)
        (liii tui box))

(check-set-mode! 'report-failed)

;; nested-layout
;; 复杂多层嵌套布局与宽度自适应综合测试

;; 1. 构建嵌套 UI 树：
;; make-box (外层边框)
;;   └─ make-vstack (垂直堆叠)
;;        ├─ make-text ("Dashboard")
;;        ├─ make-hstack (左右两列面板)
;;        │    ├─ make-box ("CPU: 25%" :style 'single :title "CPU")
;;        │    └─ make-box ("Mem: 40%" :style 'single :title "RAM")
;;        └─ make-text ("Status: Running")

(define (create-dashboard)
  (let* ((cpu-box (make-box "CPU: 25%" :style 'single :title "CPU"))
         (mem-box (make-box "Mem: 40%" :style 'single :title "RAM"))
         (panels (make-hstack (list cpu-box mem-box) :spacing 2))
         (content (make-vstack (list "Dashboard" panels "Status: Running") :spacing 1)))
    (make-box content :style 'rounded :title "System")))

;; 2. 宽度 50 下完整渲染与列对齐断言
(let* ((dash (create-dashboard))
       (lines (component-render dash 50)))
  ;; 验证总行数
  (check (> (length lines) 5) => #t)
  ;; 验证每一行的视觉显示宽度严格等于 50
  (for-each
    (lambda (line)
      (check (display-width line) => 50))
    lines)
  ;; 验证外层顶部圆角边框与标题
  (check (car lines) => "╭─ System ───────────────────────────────────────╮")
  ;; 验证外层底部圆角边框
  (check (list-ref lines (- (length lines) 1)) => "╰────────────────────────────────────────────────╯"))

;; 3. 动态响应式宽度测试：宽度从 60 收缩到 40，再到 30
(let ((dash (create-dashboard)))
  (for-each
    (lambda (w)
      (let ((rendered (component-render dash w)))
        (check (> (length rendered) 0) => #t)
        (for-each
          (lambda (line)
            (check (display-width line) => w))
          rendered)))
    '(60 45 35 25)))

;; 4. 多层嵌套输入派发与状态级联测试
(let* ((received-key #f)
       (inner-input (make-component
                      :render (lambda (w) '("input"))
                      :handle-input (lambda (k) (set! received-key k) #t)))
       (inner-box (make-box inner-input :style 'ascii))
       (row (make-hstack (list "label" inner-box)))
       (col (make-vstack (list "header" row)))
       (root (make-box col :style 'double)))
  ;; 向 root 发送按键事件
  (check (component-handle-input root 'ctrl-c) => #t)
  (check received-key => 'ctrl-c)
  ;; root 状态失效
  (check (component-invalidate! root) => #t))

(check-report)
