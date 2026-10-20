(import (liii check)
        (liii tui component))

;; component-handle-input
;; 向组件分发键盘或事件输入。
;;
;; 语法
;; ----
;; (component-handle-input comp key)
;;
;; 参数
;; ----
;; comp : component
;; 要接收输入的组件对象。
;; key : any
;; 输入事件或按键符号，如 'up、'down、'enter、#\a、"text" 等。
;;
;; 返回值
;; ----
;; any
;; 组件处理后的结果。未自定义时默认返回 #f（表示不消费）。
;;
;; 说明
;; ----
;; 若 comp 不是合法组件，抛出 type-error。

(let ((c (make-text "static text")))
  ;; make-text 默认不消费任何输入
  (check (component-handle-input c 'up) => #f)
  (check (component-handle-input c 'enter) => #f))

(let ((c (make-component
           (lambda (comp w) '())
           (lambda (comp key)
             (if (eq? key 'enter) 'submitted #f)))))
  (check (component-handle-input c 'up) => #f)
  (check (component-handle-input c 'enter) => 'submitted))

;; 错误处理
(check-catch 'type-error (component-handle-input "invalid-comp" 'up))
(check-catch 'type-error (component-handle-input 999 'enter))

(check-report)
