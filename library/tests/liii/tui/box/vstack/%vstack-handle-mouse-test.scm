;; %vstack-handle-mouse
;; 从组件数据中读取缓存的鼠标布局并派发鼠标事件。
;;
;; 语法
;; ----
;; (%vstack-handle-mouse self ev)
;;
;; 参数
;; ----
;; self : component?
;; 垂直堆叠组件自身。
;;
;; ev : mouse-event?
;; 鼠标事件。
;;
;; 返回值
;; ----
;; any
;; 派发结果；未渲染（无布局缓存）或未命中时返回 #f。

(import (scheme base)
  (liii check)
  (liii tui mouse)
  (liii tui component)
  (liii tui box mouse-region)
  (liii tui box vstack)
) ;import

(check-set-mode! 'report-failed)

(let* ((clicked #f)
       (item (make-mouse-region (make-text "btn") (lambda (ev) (set! clicked #t) #t)))
       (v (make-vstack item))
      ) ;
  ;; 未渲染前无布局缓存，返回 #f
  (check-false (%vstack-handle-mouse v (make-mouse-event 'click 'left 0 0)))
  ;; 渲染后建立布局缓存
  (component-render v 20)
  (check-true (%vstack-handle-mouse v (make-mouse-event 'click 'left 0 0)))
  (check-true clicked)
) ;let*

(check-report)
