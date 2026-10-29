(import (scheme base)
        (liii check)
        (liii list)
        (liii tui)
        (liii tui mouse)
        (liii pi code modes interactive app)
        (liii pi code modes interactive run)
        (liii pi code modes interactive views))

(check-set-mode! 'report-failed)

;; 辅助：构造带虚拟终端的 app（handler 恒成功）
(define (%make-test-app)
  (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)
    :handler (lambda (a m) #t)
  ) ;make-pi-app
) ;define

;; 1. 点击命中工具卡片 → 卡片 expanded? 取反
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (pi-app-add-tool-result! app "read" '(("path" . "a.scm")) 'success "l1\nl2\nl3")
  (pi-app-render! app)
  (let* ((card (car (filter tool-card-component? (pi-app-chat app))))
         (hit #f)
        ) ;
    (check (tool-card-expanded? card) => #f)
    ;; 逐行试探点击，找到能翻转卡片展开状态的物理行
    (for-each
      (lambda (y)
        (when (not hit)
          (pi-app-handle-mouse! app (make-mouse-event 'click 'left 5 y))
          (when (tool-card-expanded? card) (set! hit y))
        ) ;when
      ) ;lambda
      (iota 12)
    ) ;for-each
    (check-true (and hit (< hit 12)))
    ;; 再次点击同一行：折叠回去
    (pi-app-handle-mouse! app (make-mouse-event 'click 'left 5 hit))
    (check (tool-card-expanded? card) => #f)
  ) ;let*
) ;let*

;; 2. 主循环消费 click 事件：预注入点击后卡片展开，键盘语义不受影响
(let* ((probe (%make-test-app))
       (probe-card #f)
       (hit #f)
      ) ;
  (pi-app-add-tool-result! probe "read" '(("path" . "a.scm")) 'success "l1\nl2\nl3")
  (pi-app-render! probe)
  (set! probe-card (car (filter tool-card-component? (pi-app-chat probe))))
  (for-each
    (lambda (y)
      (when (not hit)
        (pi-app-handle-mouse! probe (make-mouse-event 'click 'left 5 y))
        (when (tool-card-expanded? probe-card) (set! hit y))
      ) ;when
    ) ;lambda
    (iota 12)
  ) ;for-each
  (check-true (and hit (< hit 12)))
  ;; 用同样的布局在新 app 上通过主循环派发点击
  (let* ((vt (make-virtual-terminal :width 80 :height 24))
         (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
         (ed (pi-app-editor app))
        ) ;
    (pi-app-add-tool-result! app "read" '(("path" . "a.scm")) 'success "l1\nl2\nl3")
    (let ((card (car (filter tool-card-component? (pi-app-chat app)))))
      (virtual-terminal-push-key! vt (make-mouse-event 'click 'left 5 hit))
      ;; 点击后退出
      (virtual-terminal-push-key! vt 'ctrl-d)
      (check (pi-app-run app) => #t)
      ;; 主循环消费 click 后卡片展开
      (check (tool-card-expanded? card) => #t)
      ;; 鼠标事件未被当成字符写入编辑器
      (check (editor-text ed) => "")
    ) ;let
  ) ;let
) ;let

;; 3. 连续两次点击回到折叠态
(let* ((probe (%make-test-app))
       (hit #f)
      ) ;
  (pi-app-add-tool-result! probe "read" '(("path" . "a.scm")) 'success "l1\nl2\nl3")
  (pi-app-render! probe)
  (let ((probe-card (car (filter tool-card-component? (pi-app-chat probe)))))
    (for-each
      (lambda (y)
        (when (not hit)
          (pi-app-handle-mouse! probe (make-mouse-event 'click 'left 5 y))
          (when (tool-card-expanded? probe-card) (set! hit y))
        ) ;when
      ) ;lambda
      (iota 12)
    ) ;for-each
  ) ;let
  (check-true (and hit (< hit 12)))
  (let* ((vt (make-virtual-terminal :width 80 :height 24))
         (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
        ) ;
    (pi-app-add-tool-result! app "read" '(("path" . "a.scm")) 'success "l1\nl2\nl3")
    (let ((card (car (filter tool-card-component? (pi-app-chat app)))))
      (virtual-terminal-push-key! vt (make-mouse-event 'click 'left 5 hit))
      (virtual-terminal-push-key! vt (make-mouse-event 'click 'left 5 hit))
      (virtual-terminal-push-key! vt 'ctrl-d)
      (check (pi-app-run app) => #t)
      (check (tool-card-expanded? card) => #f)
    ) ;let
  ) ;let
) ;let

(check-report)
