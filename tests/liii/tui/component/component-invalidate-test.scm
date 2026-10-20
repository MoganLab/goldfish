(import (liii check)
        (liii tui component))

;; component-invalidate!
;; 标记组件脏区或刷新内部状态。
;;
;; 语法
;; ----
;; (component-invalidate! comp)
;;
;; 参数
;; ----
;; comp : component
;; 要标记或刷新的组件。
;;
;; 返回值
;; ----
;; any
;; 组件 invalidate 过程的返回值，默认返回 #t。
;;
;; 说明
;; ----
;; 当组件状态改变、需要重新渲染或通知上层刷新时调用。
;; 若 comp 不是合法组件，抛出 type-error。

(let ((c (make-text "hello")))
  (check (component-invalidate! c) => #t))

(let* ((dirty #f)
       (c (make-component
            (lambda (comp w) '())
            (lambda (comp k) #f)
            (lambda (comp)
              (set! dirty #t)
              'dirty-marked))))
  (check dirty => #f)
  (check (component-invalidate! c) => 'dirty-marked)
  (check dirty => #t))

;; 错误处理
(check-catch 'type-error (component-invalidate! "string"))
(check-catch 'type-error (component-invalidate! '()))

(check-report)
