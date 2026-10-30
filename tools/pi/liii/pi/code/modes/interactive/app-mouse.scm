;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License.
;;

;; 鼠标事件接入：把屏幕坐标的鼠标事件换算为 doc 逻辑行后派发给 doc 组件树，
;; 由各卡片自带的 mouse-region 回调完成点击切换展开（对齐上游 pi
;; tool-execution.ts 的 createResultRegion：click + left 且已完成时翻转）。
;;
;; 全屏（备用屏）布局下，帧自上而下为：
;;   [对话记录视口]  占据帧顶部，内部按 scroll-view 偏移滚动
;;   [待发送队列卡片]
;;   [输入区]
;;   [斜杠命令菜单]
;;   [页脚]
;; 因此屏幕行 y 必须落在视口高度内才可能命中卡片，
;; doc 逻辑行 = y + 视口滚动偏移（scroll-view-offset）。

(define-library (liii pi code modes interactive app-mouse)
  (export pi-app-dispatch-mouse!)
  (import (scheme base)
    (liii tui)
    (liii tui mouse)
    (liii tui scroll-view)
    (liii pi code modes interactive state)
  ) ;import
  (begin

    ;; 派发鼠标事件到 doc 组件树：命中返回 #t，未命中返回 #f
    (define (pi-app-dispatch-mouse! app ev)
      (let* ((sv (pi-app-scroll-view app))
             (h (scroll-view-height sv))
             (y (mouse-event-y ev))
            ) ;
        (if (or (<= h 0) (>= y h))
          #f
          (component-handle-mouse! (%pi-app-doc app)
            (make-mouse-event (mouse-event-type ev)
              (mouse-event-button ev)
              (mouse-event-x ev)
              (+ y (scroll-view-offset sv))
              (mouse-event-shift? ev)
              (mouse-event-ctrl? ev)
              (mouse-event-alt? ev)
              (mouse-event-time ev)
            ) ;make-mouse-event
          ) ;component-handle-mouse!
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
