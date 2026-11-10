;; pi-scroll-hint-rect
;; 测试全屏交互模式下离开末尾跟随状态时，底部“跳到最新消息”提示热区坐标的几何计算与滚动条避让。
;;
;; 语法
;; ----
;; (pi-scroll-hint-rect)
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
  (liii string)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

;; 1. 80x24 标准终端：跟随末尾时热区为 #f；滚离末尾时提示热区位于视口末行且水平居中
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
  ;; 初始跟随末尾：无热区
  (pi-app-render! app)
  (check-false (pi-app-scroll-hint-rect app))
  ;; 向上滚动脱离末尾：出现热区
  (pi-app-scroll-up! app)
  (pi-app-render! app)
  (let ((rect (pi-app-scroll-hint-rect app)))
    ;; 80x24 终端视口高 19 行（row 0..18），提示位于末行 row 18
    ;; 提示文本可见宽 37，(quotient (- 80 37) 2) = 21，右界 21 + 37 = 58
    (check (car rect) => 18)
    (check (cadr rect) => 21)
    (check (caddr rect) => 37)
    ;; 验证未遮挡右侧滚动条（最右列 79）
    (check-true (< (+ (cadr rect) (caddr rect)) 79))
  ) ;let
  ;; 回到底部：热区重置为 #f
  (pi-app-scroll-bottom! app)
  (pi-app-render! app)
  (check-false (pi-app-scroll-hint-rect app))
) ;let*

;; 2. 100x30 大终端：行号与居中列自适应
(let* ((vterm (make-virtual-terminal :width 100 :height 30))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
      ) ;
  (let loop
    ((i 0))
    (when (< i 40)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-scroll-up! app)
  (pi-app-render! app)
  (let ((rect (pi-app-scroll-hint-rect app)))
    ;; 30 行终端视口高 25 行（row 0..24），提示位于末行 row 24
    ;; 100 宽居中：(quotient (- 100 37) 2) = 31，右界 31 + 37 = 68
    (check (car rect) => 24)
    (check (cadr rect) => 31)
    (check (caddr rect) => 37)
    (check-true (< (+ (cadr rect) (caddr rect)) 99))
  ) ;let
) ;let*

;; 3. 40x24 紧凑终端：居中起始列为 1，右界为 38，不覆盖滚动条所在列 39
(let* ((vterm (make-virtual-terminal :width 40 :height 24))
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
  (let ((rect (pi-app-scroll-hint-rect app)))
    (check (car rect) => 18)
    (check (cadr rect) => 1)
    (check (caddr rect) => 37)
    (check-true (<= (+ (cadr rect) (caddr rect)) 39))
  ) ;let
) ;let*

;; 4. 36x24 窄终端：文本超出可用宽度时，右边界主动避让最右侧滚动条所在列（列 35）
(let* ((vterm (make-virtual-terminal :width 36 :height 24))
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
  (let ((rect (pi-app-scroll-hint-rect app)))
    (check (car rect) => 18)
    (check (cadr rect) => 0)
    ;; 36 宽避开列 35，热区有效宽度钳制为 35
    (check (caddr rect) => 35)
    (check-true (<= (+ (cadr rect) (caddr rect)) 35))
  ) ;let
) ;let*

(check-report)
