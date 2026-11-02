(import (liii check)
        (liii tui text-width)
        (liii tui component)
        (liii tui box))

(check-set-mode! 'report-failed)

;; make-vstack
;; 垂直堆叠布局组件，将多个子组件纵向依次拼接排版

;; 1. 基础创建与 Component 协议检查
(let ((v (make-vstack (list (make-text "line 1") (make-text "line 2")))))
  (check (component? v) => #t)
  (check (vstack? v) => #t))

;; 支持不定参数（可直接传入子组件列表，或以多个位置参数传入）
(let ((v (make-vstack "line 1" "line 2")))
  (check (component? v) => #t)
  (check (vstack? v) => #t))

;; 2. 垂直排列渲染
(let* ((v (make-vstack "first" "second"))
       (lines (component-render v 20)))
  (check (length lines) => 2)
  (check (car lines) => "first")
  (check (cadr lines) => "second"))

;; 3. 间距 spacing 支持
;; spacing = 1 在相邻子组件间插入 1 个空行
(let* ((v (make-vstack (list "first" "second") :spacing 1))
       (lines (component-render v 20)))
  (check (length lines) => 3)
  (check (car lines) => "first")
  (check (cadr lines) => "")
  (check (caddr lines) => "second"))

;; spacing = 2 插入 2 个空行
(let* ((v (make-vstack "a" "b" "c" :spacing 2))
       (lines (component-render v 20)))
  (check (length lines) => 7)
  (check (list-ref lines 0) => "a")
  (check (list-ref lines 1) => "")
  (check (list-ref lines 2) => "")
  (check (list-ref lines 3) => "b")
  (check (list-ref lines 4) => "")
  (check (list-ref lines 5) => "")
  (check (list-ref lines 6) => "c"))

;; 4. 宽度透传：子组件按给定宽度折行
(let* ((v (make-vstack "hello world" "foo bar"))
       (lines (component-render v 5)))
  ;; 宽度为 5 时分别折行
  (check lines => '("hello" "world" "foo" "bar")))

;; 5. 空子组件边界处理
(let ((v (make-vstack '())))
  (check (component-render v 20) => '()))

;; 6. 输入事件级联下发（遇到第一个消费的子组件即中止）
(let* ((order '())
       (c1 (make-component
             :render (lambda (w) '("c1"))
             :handle-input (lambda (k) (set! order (cons 'c1 order)) #f)))
       (c2 (make-component
             :render (lambda (w) '("c2"))
             :handle-input (lambda (k) (set! order (cons 'c2 order)) 'handled)))
       (c3 (make-component
             :render (lambda (w) '("c3"))
             :handle-input (lambda (k) (set! order (cons 'c3 order)) #t)))
       (v (make-vstack (list c1 c2 c3))))
  (check (component-handle-input v 'enter) => 'handled)
  ;; c1 与 c2 被调用，c3 未被调用
  (check (reverse order) => '(c1 c2)))

;; 全部未消费时返回 #f
(let* ((c1 (make-component :render (lambda (w) '("c1"))))
       (v (make-vstack (list c1))))
  (check (component-handle-input v 'up) => #f))

;; 7. 刷新与失效通知级联
(let* ((inv-count 0)
       (c1 (make-component
             :render (lambda (w) '("c1"))
             :invalidate (lambda () (set! inv-count (+ inv-count 1)) #t)))
       (c2 (make-component
             :render (lambda (w) '("c2"))
             :invalidate (lambda () (set! inv-count (+ inv-count 1)) #t)))
       (v (make-vstack (list c1 c2))))
  (check (component-invalidate! v) => #t)
  (check inv-count => 2))

;; 8. 异常与错误拦截
(check-catch 'type-error (make-vstack (list 123)))
(check-catch 'type-error (make-vstack "a" :spacing "1"))
(check-catch 'value-error (make-vstack "a" :spacing -1))

(check-report)
