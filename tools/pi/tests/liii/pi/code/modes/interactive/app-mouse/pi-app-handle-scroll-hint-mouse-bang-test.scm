;; pi-app-handle-scroll-hint-mouse!
;; 检测鼠标左键按压事件是否命中当前帧“跳到最新消息”提示行热区并触发跳转。
;;
;; 语法
;; ----
;; (pi-app-handle-scroll-hint-mouse! app ev)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; ev : mouse-event?
;; 鼠标事件对象。
;;
;; 返回值
;; -----
;; boolean?
;; 命中热区并成功触发跳转返回 #t，未命中或非左键按下返回 #f。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii tui mouse)
  (liii pi code modes interactive app)
  (liii pi code modes interactive app-mouse)
  (liii pi code modes interactive state)
  (liii pi code modes interactive selection)
) ;import

(check-set-mode! 'report-failed)

;; 1. 跟随末尾（无提示热区）时，任意鼠标事件均返回 #f 且不触发滚动
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (sv (pi-app-scroll-view app))
      ) ;
  (let loop
    ((i 0))
    (when (< i 30)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-render! app)
  (check-true (scroll-view-follow? sv))
  (check-false (pi-app-scroll-hint-rect app))
  (check-false
    (pi-app-handle-scroll-hint-mouse! app (make-mouse-event 'press 'left 30 18))
  ) ;check-false
  (check-true (scroll-view-follow? sv))
) ;let*

;; 2. 脱离跟随末尾时，命中热区的 left press 事件触发跳转并返回 #t
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (sv (pi-app-scroll-view app))
      ) ;
  (let loop
    ((i 0))
    (when (< i 30)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-scroll-up! app)
  (pi-app-render! app)
  (check-false (scroll-view-follow? sv))
  (let ((rect (pi-app-scroll-hint-rect app)))
    (check-true (pair? rect))
    (let ((row (car rect)) (col (cadr rect)) (w (caddr rect)))
      ;; 命中左边缘
      (check-true
        (pi-app-handle-scroll-hint-mouse! app (make-mouse-event 'press 'left col row))
      ) ;check-true
      (check-true (scroll-view-follow? sv))
      ;; 重新滚离末尾验证中间位置
      (pi-app-scroll-up! app)
      (pi-app-render! app)
      (check-true
        (pi-app-handle-scroll-hint-mouse! app
          (make-mouse-event 'press 'left (+ col (quotient w 2)) row)
        ) ;pi-app-handle-scroll-hint-mouse!
      ) ;check-true
      (check-true (scroll-view-follow? sv))
      ;; 重新滚离末尾验证右边缘
      (pi-app-scroll-up! app)
      (pi-app-render! app)
      (check-true
        (pi-app-handle-scroll-hint-mouse! app
          (make-mouse-event 'press 'left (+ col w -1) row)
        ) ;pi-app-handle-scroll-hint-mouse!
      ) ;check-true
      (check-true (scroll-view-follow? sv))
    ) ;let
  ) ;let
) ;let*

;; 3. 热区之外的坐标不触发跳转，返回 #f
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (sv (pi-app-scroll-view app))
      ) ;
  (let loop
    ((i 0))
    (when (< i 30)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-scroll-up! app)
  (pi-app-render! app)
  (let* ((rect (pi-app-scroll-hint-rect app))
         (row (car rect))
         (col (cadr rect))
         (w (caddr rect))
        ) ;
    ;; 热区左侧空白
    (check-false
      (pi-app-handle-scroll-hint-mouse! app (make-mouse-event 'press 'left (- col 1) row))
    ) ;check-false
    (check-false (scroll-view-follow? sv))
    ;; 热区右侧空白
    (check-false
      (pi-app-handle-scroll-hint-mouse! app (make-mouse-event 'press 'left (+ col w) row))
    ) ;check-false
    (check-false (scroll-view-follow? sv))
    ;; 最右侧滚动条所在列
    (check-false
      (pi-app-handle-scroll-hint-mouse! app (make-mouse-event 'press 'left 79 row))
    ) ;check-false
    (check-false (scroll-view-follow? sv))
    ;; 上方行与下方行
    (check-false
      (pi-app-handle-scroll-hint-mouse! app (make-mouse-event 'press 'left col (- row 1)))
    ) ;check-false
    (check-false
      (pi-app-handle-scroll-hint-mouse! app (make-mouse-event 'press 'left col (+ row 1)))
    ) ;check-false
    (check-false (scroll-view-follow? sv))
  ) ;let*
) ;let*

;; 4. 非 left 或非 press 事件不触发跳转，返回 #f
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (sv (pi-app-scroll-view app))
      ) ;
  (let loop
    ((i 0))
    (when (< i 30)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-scroll-up! app)
  (pi-app-render! app)
  (let* ((rect (pi-app-scroll-hint-rect app))
         (row (car rect))
         (col (cadr rect))
         (mid (+ col 5))
        ) ;
    ;; 右键或中键按下
    (check-false
      (pi-app-handle-scroll-hint-mouse! app (make-mouse-event 'press 'right mid row))
    ) ;check-false
    (check-false
      (pi-app-handle-scroll-hint-mouse! app (make-mouse-event 'press 'middle mid row))
    ) ;check-false
    ;; release, drag, click 事件
    (check-false
      (pi-app-handle-scroll-hint-mouse! app (make-mouse-event 'release 'left mid row))
    ) ;check-false
    (check-false
      (pi-app-handle-scroll-hint-mouse! app (make-mouse-event 'drag 'left mid row))
    ) ;check-false
    (check-false
      (pi-app-handle-scroll-hint-mouse! app (make-mouse-event 'click 'left mid row))
    ) ;check-false
    (check-false (scroll-view-follow? sv))
  ) ;let*
) ;let*

;; 5. 命中跳转时清空已有选区
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
      ) ;
  (let loop
    ((i 0))
    (when (< i 30)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-scroll-up! app)
  (pi-app-render! app)
  (pi-app-selection-set! app (make-selection-range '(0 . 0) '(10 . 5) #t))
  (check-true (pi-app-selection-active? app))
  (let* ((rect (pi-app-scroll-hint-rect app)) (row (car rect)) (col (cadr rect)))
    (check-true
      (pi-app-handle-scroll-hint-mouse! app (make-mouse-event 'press 'left col row))
    ) ;check-true
    (check-false (pi-app-selection app))
    (check-false (pi-app-selection-active? app))
  ) ;let*
) ;let*

(check-report)
