;; make-mouse-region
;; 为任意组件附加鼠标事件拦截与回调处理能力。
;;
;; 语法
;; ----
;; (make-mouse-region child on-mouse)
;;
;; 参数
;; ----
;; child : component?
;; 被包装的目标子组件。
;;
;; on-mouse : procedure?
;; 鼠标事件回调过程，接收 mouse-event 对象作为参数。
;;
;; 返回值
;; ----
;; component?
;; 包装后的 mouse-region 组件实例。

(import (scheme base)
  (liii check)
  (liii tui mouse)
  (liii tui component)
  (liii tui box mouse-region)
) ;import

(check-set-mode! 'report-failed)

(let* ((child (make-text "click me"))
       (clicked #f)
       (mr (make-mouse-region child (lambda (ev) (set! clicked #t) 'handled)))
      ) ;
  (check-true (component? mr))
  (check-true (mouse-region? mr))
  ;; 渲染与普通组件一致
  (check (component-render mr 20) => '("click me"))
  ;; 收到鼠标事件触发回调
  (check (component-handle-mouse! mr (make-mouse-event 'click 'left 0 0))
    =>
    'handled
  ) ;check
  (check-true clicked)
) ;let*

(check-catch 'type-error (make-mouse-region "not-a-comp" (lambda (ev) #t)))
(check-catch 'type-error (make-mouse-region (make-text "x") "not-a-proc"))

(check-report)
