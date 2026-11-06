;; %pi-app-handle-resize!
;; 主循环每轮调用的终端缩放检测：尺寸变化时按新尺寸重新布局并重绘整帧；
;; 无变化或首次记录基准时不重绘。
;;
;; 语法
;; ----
;; (%pi-app-handle-resize! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 待检测缩放的交互式应用状态。
;;
;; 返回值
;; ----
;; 未指定
;; 该过程通过副作用更新屏幕尺寸记录，并在检测到变化时触发整帧重绘。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii tui text-width)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive render)
) ;import

(check-set-mode! 'report-failed)

;; 首次调用仅记录基准尺寸：无变化不重绘，帧保持当前终端尺寸
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (screen (pi-app-screen app))
      ) ;
  (pi-app-render! app)
  (%pi-app-handle-resize! app)
  (check (length (alt-screen-lines screen)) => 24)
  (%pi-app-handle-resize! app)
  (check (length (alt-screen-lines screen)) => 24)
) ;let*

;; 终端缩放后：检测到变化并按新尺寸重新布局重绘
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (screen (pi-app-screen app))
      ) ;
  (pi-app-render! app)
  (%pi-app-handle-resize! app)
  (virtual-terminal-resize! vt 60 12)
  (%pi-app-handle-resize! app)
  ;; 帧已按新终端重排：行数等于新高度，行宽等于新宽度
  (check (length (alt-screen-lines screen)) => 12)
  (check
    (apply max (map display-width (map strip-ansi (alt-screen-lines screen))))
    =>
    60
  ) ;check
  ;; 编辑器高度按新终端高度重新计算
  (check (editor-max-lines (pi-app-editor app)) => 4)
  ;; 尺寸稳定后再次调用：无变化，帧保持不变
  (%pi-app-handle-resize! app)
  (check (length (alt-screen-lines screen)) => 12)
) ;let*

(check-report)
