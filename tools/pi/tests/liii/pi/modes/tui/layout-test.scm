(import (liii check)
  (liii error)
  (liii tui main-screen)
  (liii tui terminal)
  (liii pi modes tui layout)
) ;import

(check-set-mode! 'report-failed)

;; ==========================================
;; 1. compute-editor-height: 参数检查与高度约束
;; ==========================================
(check-catch 'type-error (compute-editor-height 0 1))
(check-catch 'type-error (compute-editor-height -10 1))
(check-catch 'type-error (compute-editor-height 24 -1))
(check-catch 'type-error (compute-editor-height "24" 1))

;; 常规 24 行终端：
;; 1 行输入 -> 1+2 = 3 行
(check (compute-editor-height 24 1) => 3)
;; 2 行输入 -> 2+2 = 4 行
(check (compute-editor-height 24 2) => 4)
;; 5 行输入 -> 5+2 = 7 行 <= 24*0.4(9) -> 7 行
(check (compute-editor-height 24 5) => 7)
;; 20 行超长输入 -> 受限于 24*0.4 = 9 行
(check (compute-editor-height 24 20) => 9)

;; 极小屏幕行数约束
;; 5 行屏幕：max-allowed 为 3，2 行输入时 2+2=4 受限于 (floor (* 5 0.4)) 无法成立但保底 3
(check (compute-editor-height 5 1) => 3)
(check (compute-editor-height 5 4) => 3)
;; 2 行屏幕：不超过 rows -> 2
(check (compute-editor-height 2 1) => 2)

;; ==========================================
;; 2. compute-tui-layout: 三段式拓扑划分
;; ==========================================
(check-catch 'type-error (compute-tui-layout 0 24))
(check-catch 'type-error (compute-tui-layout 80 0))
(check-catch 'type-error (compute-tui-layout "80" 24))

(let ((l (compute-tui-layout 80 24 1)))
  (check (tui-layout? l) => #t)
  (check (layout-cols l) => 80)
  (check (layout-rows l) => 24)
  (check (layout-narrow? l) => #f)
  (check (layout-compact? l) => #f)

  ;; 底部状态栏高度为 1
  (let ((st (layout-status-rect l)))
    (check (tui-rect? st) => #t)
    (check (rect-top st) => 23)
    (check (rect-left st) => 0)
    (check (rect-width st) => 80)
    (check (rect-height st) => 1)
  ) ;let

  ;; 输入框高度为 3 (1+2)
  (let ((ed (layout-editor-rect l)))
    (check (tui-rect? ed) => #t)
    (check (rect-height ed) => 3)
    (check (rect-top ed) => 20)
    (check (rect-left ed) => 0)
    (check (rect-width ed) => 80)
  ) ;let

  ;; 消息视口占满剩余高度：24 - 1 - 3 = 20
  (let ((msg (layout-messages-rect l)))
    (check (tui-rect? msg) => #t)
    (check (rect-top msg) => 0)
    (check (rect-left msg) => 0)
    (check (rect-width msg) => 80)
    (check (rect-height msg) => 20)
  ) ;let
) ;let

;; ==========================================
;; 3. 窄屏与紧凑屏幕模式判定
;; ==========================================
;; 宽度 < 60 列为 narrow 模式
(let ((l (compute-tui-layout 50 24 1)))
  (check (layout-narrow? l) => #t)
  (check (layout-compact? l) => #t)
) ;let

;; 宽度 >= 60 但高度 < 15 行进入 compact 模式
(let ((l (compute-tui-layout 80 12 1)))
  (check (layout-narrow? l) => #f)
  (check (layout-compact? l) => #t)
  ;; 状态栏仍为 1 行，输入框最小 3 行，剩余 8 行给消息
  (check (rect-height (layout-status-rect l)) => 1)
  (check (rect-height (layout-editor-rect l)) => 3)
  (check (rect-height (layout-messages-rect l)) => 8)
) ;let

;; 极小高度屏幕 (< 10 行) 折叠状态栏为 0 行
(let ((l (compute-tui-layout 80 8 1)))
  (check (layout-compact? l) => #t)
  (check (rect-height (layout-status-rect l)) => 0)
  (check (rect-height (layout-editor-rect l)) => 3)
  (check (rect-height (layout-messages-rect l)) => 5)
) ;let

;; ==========================================
;; 4. detect-terminal-dimensions
;; ==========================================
(check-catch 'type-error (detect-terminal-dimensions "not-term"))

(let ((vt (make-virtual-terminal :width 100 :height 30)))
  (let-values (((w h) (detect-terminal-dimensions vt)))
    (check w => 100)
    (check h => 30)
  ) ;let-values
) ;let

;; ==========================================
;; 5. handle-screen-resize!: 与 main-screen 联动
;; ==========================================
(check-catch 'type-error (handle-screen-resize! "not-screen" 80 24 100 30))
(check-catch 'type-error (handle-screen-resize! (make-main-screen (make-virtual-terminal)) 80 24 0 30))
(check-catch 'type-error (handle-screen-resize! (make-main-screen (make-virtual-terminal)) 80 24 100 -5))

(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (ms (make-main-screen vt))
      ) ;
  ;; 尺寸未变时返回 #f
  (check (handle-screen-resize! ms 80 24 80 24) => #f)

  ;; 尺寸发生变化时返回 #t，并成功更新虚拟终端与 main-screen 内部状态
  (check (handle-screen-resize! ms 80 24 120 40) => #t)
  (let ((sz (terminal-size vt)))
    (check (car sz) => 120)
    (check (cadr sz) => 40)
  ) ;let
) ;let

(check-report)