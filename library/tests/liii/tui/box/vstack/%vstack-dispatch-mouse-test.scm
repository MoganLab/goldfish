;; %vstack-dispatch-mouse
;; 根据预计算的高度区间命中子组件，并将鼠标事件换算为子组件局部坐标派发。
;;
;; 语法
;; ----
;; (%vstack-dispatch-mouse children heights spacing ev)
;;
;; 参数
;; ----
;; children : list?
;; 子组件列表。
;;
;; heights : list?
;; 各子组件行高列表。
;;
;; spacing : integer?
;; 间距行数。
;;
;; ev : mouse-event?
;; 鼠标事件对象。
;;
;; 返回值
;; ----
;; any
;; 命中子组件的处理结果；未命中时返回 #f。

(import (scheme base)
  (liii check)
  (liii tui mouse)
  (liii tui component)
  (liii tui box mouse-region)
  (liii tui box vstack)
) ;import

(check-set-mode! 'report-failed)

(let* ((clicked #f)
       (c1 (make-mouse-region (make-text "a") (lambda (ev) (set! clicked 'c1) #t)))
       (c2 (make-mouse-region (make-text "b") (lambda (ev) (set! clicked 'c2) #t)))
       (children (list c1 c2))
       (heights '(1 1))
      ) ;
  ;; 点击第 0 行命中 c1
  (check-true (%vstack-dispatch-mouse children heights 0 (make-mouse-event 'click 'left 0 0))
  ) ;check-true
  (check clicked => 'c1)
  ;; 点击第 1 行命中 c2
  (check-true (%vstack-dispatch-mouse children heights 0 (make-mouse-event 'click 'left 0 1))
  ) ;check-true
  (check clicked => 'c2)
  ;; 超出高度不命中
  (check-false (%vstack-dispatch-mouse children heights 0 (make-mouse-event 'click 'left 0 5))
  ) ;check-false
) ;let*

(check-report)
