;; component-handle-mouse!
;; 向组件分发鼠标事件。
;;
;; 语法
;; ----
;; (component-handle-mouse! comp ev)
;;
;; 参数
;; ----
;; comp : component?
;; 目标组件。
;;
;; ev : mouse-event?
;; 鼠标事件对象。
;;
;; 返回值
;; ----
;; any
;; 组件内部 handle-mouse 过程的处理结果；未处理时默认返回 #f。

(import (scheme base) (liii check) (liii tui mouse) (liii tui component))

(check-set-mode! 'report-failed)

;; 1. 默认组件不处理鼠标事件
(let ((c (make-text "text")))
  (check-false (component-handle-mouse! c (make-mouse-event 'click 'left 0 0)))
) ;let

;; 2. 自定义 handle-mouse 接收并处理事件
(let* ((handled #f)
       (c (make-component
            :render       (lambda (comp w) '("test"))
            :handle-mouse (lambda (comp ev) (set! handled ev) #t)
          ) ;make-component
       ) ;c
      ) ;
  (let ((ev (make-mouse-event 'click 'left 2 3)))
    (check-true (component-handle-mouse! c ev))
    (check handled => ev)
  ) ;let
) ;let*

;; 3. 类型错误
(check-catch 'type-error
  (component-handle-mouse! "not-a-comp" (make-mouse-event 'click 'left 0 0))
) ;check-catch
(check-catch 'type-error
  (component-handle-mouse! (make-spacer 1) "not-an-event")
) ;check-catch

(check-report)
