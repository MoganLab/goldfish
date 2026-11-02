(import (scheme base)
        (liii check)
        (liii list)
        (liii string)
        (liii tui)
        (liii tui mouse)
        (liii pi code modes interactive app)
        (liii pi code modes interactive run)
        (liii pi code modes interactive state)
        (liii pi code modes interactive selection))

(check-set-mode! 'report-failed)

;; 辅助：构造带虚拟终端的 app
(define (%make-test-app)
  (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)
    :handler (lambda (a m) #t)
  ) ;make-pi-app
) ;define

;; 1. 鼠标按下、拖拽与释放的选区状态机驱动
(let* ((app (%make-test-app)))
  ;; 初始无选区
  (check-false (pi-app-selection-active? app))
  ;; 左键 press：记录候选锚点，未激活
  (pi-app-handle-mouse! app (make-mouse-event 'press 'left 5 5))
  (check-false (pi-app-selection-active? app))
  (let ((sel (pi-app-selection app)))
    (check (selection-anchor sel) => '(5 . 5))
    (check (selection-focus sel) => '(5 . 5)))
  ;; drag 位移：激活选区并更新焦点
  (pi-app-handle-mouse! app (make-mouse-event 'drag 'left 20 8))
  (check-true (pi-app-selection-active? app))
  (let ((sel (pi-app-selection app)))
    (check (selection-anchor sel) => '(5 . 5))
    (check (selection-focus sel) => '(20 . 8)))
  ;; release：固化选区，依然保持激活
  (pi-app-handle-mouse! app (make-mouse-event 'release 'left 20 8))
  (check-true (pi-app-selection-active? app)))

;; 2. 主循环响应 Escape 清除选区
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t))))
  ;; 先通过鼠标拖拽建立激活选区
  (pi-app-handle-mouse! app (make-mouse-event 'press 'left 5 5))
  (pi-app-handle-mouse! app (make-mouse-event 'drag 'left 20 8))
  (pi-app-handle-mouse! app (make-mouse-event 'release 'left 20 8))
  (check-true (pi-app-selection-active? app))
  ;; 注入 Escape，然后注入 ctrl-d 退出
  (virtual-terminal-push-key! vt 'escape)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  ;; 选区在 Escape 后已被清空
  (check-false (pi-app-selection-active? app)))

;; 3. 空白处单击清除既有选区
(let* ((app (%make-test-app)))
  (pi-app-handle-mouse! app (make-mouse-event 'press 'left 5 5))
  (pi-app-handle-mouse! app (make-mouse-event 'drag 'left 20 8))
  (pi-app-handle-mouse! app (make-mouse-event 'release 'left 20 8))
  (check-true (pi-app-selection-active? app))
  ;; 在空白处单击（press + click）
  (pi-app-handle-mouse! app (make-mouse-event 'press 'left 0 0))
  (pi-app-handle-mouse! app (make-mouse-event 'click 'left 0 0))
  (check-false (pi-app-selection-active? app)))

;; 4. 主循环消费 shift-ctrl-c 触发复制
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t))))
  (pi-app-add-user-message! app "Hello Goldfish Scheme")
  (pi-app-render! app)
  (let-values (((frame _) (pi-app-frame-lines app)))
    (let loop ((y 0))
      (if (< y (length frame))
        (let ((line (string-trim-both (strip-ansi (list-ref frame y)))))
          (if (> (string-length line) 0)
            (begin
              (virtual-terminal-push-key! vt (make-mouse-event 'press 'left 0 y))
              (virtual-terminal-push-key! vt (make-mouse-event 'drag 'left 20 y))
              (virtual-terminal-push-key! vt (make-mouse-event 'release 'left 20 y))
              (virtual-terminal-push-key! vt 'shift-ctrl-c)
              (virtual-terminal-push-key! vt 'ctrl-d)
              (check (pi-app-run app) => #t)
              (let ((raw (virtual-terminal-raw vt)))
                (check-true (any (lambda (str) (string-contains str "\x1b;]52;c;")) raw))))
            (loop (+ y 1))))))))

(check-report)
