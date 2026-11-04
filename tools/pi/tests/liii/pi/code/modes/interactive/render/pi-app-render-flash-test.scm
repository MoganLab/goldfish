;; pi-app-render-flash
;; 验证当应用设置瞬态浮层提示（Flash）时，帧渲染在屏幕第 0 行右上角正确合成反显浮层且行宽严格对齐。
;;
;; 语法
;; ----
;; (pi-app-frame-lines app)
;;
;; 参数
;; ----
;; app : pi-app?
;;
;; 返回值
;; -----
;; (values list pair)

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii tui text-width)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  ;; 未设置 flash 时，第 0 行不含 Copied!
  (let-values (((frame _) (pi-app-frame-lines app)))
    (check-false (string-contains (car frame) "Copied!"))
  ) ;let-values

  ;; 设置 flash 后，第 0 行右上角出现反显 Copied!
  (pi-app-flash! app "Copied!" 1000)
  (let-values (((frame _) (pi-app-frame-lines app)))
    (let ((line0 (car frame)))
      (check-true (string-contains line0 "\x1b;[7m Copied! \x1b;[27m"))
      ;; 视觉宽度必须严格保真为 80，不可溢出或折行
      (check (display-width line0) => 80)
    ) ;let
  ) ;let-values
) ;let*

(check-report)
