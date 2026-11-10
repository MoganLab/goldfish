;; pi-app-handle-mouse-scroll-hint
;; 测试脱离末尾时鼠标在分发流中点击跳转提示行，完整链路触发跳转至末尾、拦截选区并阻止派发至底层卡片。
;;
;; 语法
;; ----
;; (pi-app-handle-mouse-scroll-hint)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; any

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii tui mouse)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive views)
  (liii pi code modes interactive selection)
) ;import

(check-set-mode! 'report-failed)

;; 辅助：检查首张工具卡片的展开状态
(define (%tool-card-expanded? chat)
  (let ((tools (filter tool-card-component? chat)))
    (and (pair? tools) (tool-card-expanded? (car tools)))
  ) ;let
) ;define

;; 1. 鼠标点击提示行：立即滚动回末尾、消费事件、不创建激活选区且不派发给底层卡片
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (sv (pi-app-scroll-view app))
       (chat (pi-app-chat app))
      ) ;
  ;; 添加工具卡片（默认折叠）与多条消息超出一屏
  (pi-app-add-tool-result! app "read" '(("path" . "a.scm")) 'success "l1\nl2\nl3")
  (let loop
    ((i 0))
    (when (< i 25)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-render! app)
  (check-false (%tool-card-expanded? chat))
  ;; 滚离末尾，出现跳转提示行
  (pi-app-scroll-up! app)
  (pi-app-render! app)
  (check-false (scroll-view-follow? sv))
  (let* ((rect (pi-app-scroll-hint-rect app))
         (row (car rect))
         (col (cadr rect))
         (hit-x (+ col 5))
        ) ;
    ;; 向提示热区发送 press left 事件
    (check-true
      (pi-app-handle-mouse! app (make-mouse-event 'press 'left hit-x row))
    ) ;check-true
    ;; 验证已滚回末尾恢复跟随
    (check-true (scroll-view-follow? sv))
    ;; 验证事件被完全消费：选区未被设置或激活
    (check-false (pi-app-selection-active? app))
    (check-false (pi-app-selection app))
    ;; 验证底层卡片状态未被误翻转
    (check-false (%tool-card-expanded? chat))
  ) ;let*
) ;let*

;; 2. 点击提示行左右两侧空白区域：不触发跳转，正常进入选区流程
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (sv (pi-app-scroll-view app))
      ) ;
  (let loop
    ((i 0))
    (when (< i 25)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-scroll-up! app)
  (pi-app-render! app)
  (check-false (scroll-view-follow? sv))
  (let* ((rect (pi-app-scroll-hint-rect app)) (row (car rect)))
    ;; 点击左侧空白列 (x = 2)
    (pi-app-handle-mouse! app (make-mouse-event 'press 'left 2 row))
    ;; 视口未跳回末尾
    (check-false (scroll-view-follow? sv))
    ;; 正常进入选区流程，记录锚点
    (let ((sel (pi-app-selection app)))
      (check-true (selection-range? sel))
      (check (selection-anchor sel) => (cons 2 row))
    ) ;let
  ) ;let*
) ;let*

(check-report)
