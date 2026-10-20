(import (liii check)
        (liii tui component))

;; make-component
;; 创建一个自定义 TUI 组件。
;;
;; 语法
;; ----
;; (make-component render-fn [handle-input-fn] [invalidate-fn])
;; (make-component :render render-fn [:handle-input handle-input-fn] [:invalidate invalidate-fn])
;;
;; 参数
;; ----
;; render-fn : procedure
;; 渲染过程，接收组件自身与可用宽度 (comp width) 或单参数 (width)，返回行列表 (list of string)。
;; handle-input-fn : procedure, 可选
;; 输入处理过程，接收 (comp key) 或 (key)，返回布尔值或更新结果，默认为不消费返回 #f。
;; invalidate-fn : procedure, 可选
;; 刷新/标记脏区过程，接收 (comp) 或无参，默认为返回 #t。
;;
;; 返回值
;; ----
;; component
;; 新创建的组件实例。

;; 位置参数构造
(let ((c (make-component (lambda (comp width) (list "render1")))))
  (check (component? c) => #t)
  (check (component-render c 80) => '("render1"))
  (check (component-handle-input c 'enter) => #f)
  (check (component-invalidate! c) => #t))

(let ((c (make-component
           (lambda (comp width) (list (number->string width)))
           (lambda (comp key) (eq? key 'enter))
           (lambda (comp) 'invalidated))))
  (check (component? c) => #t)
  (check (component-render c 40) => '("40"))
  (check (component-handle-input c 'enter) => #t)
  (check (component-handle-input c 'space) => #f)
  (check (component-invalidate! c) => 'invalidated))

;; 关键字参数构造
(let ((c (make-component
           :render (lambda (comp w) (list "kw"))
           :handle-input (lambda (comp k) k)
           :invalidate (lambda (comp) #t))))
  (check (component? c) => #t)
  (check (component-render c 10) => '("kw"))
  (check (component-handle-input c 'esc) => 'esc))

;; 非法参数错误
(check-catch 'type-error (make-component "not-a-procedure"))
(check-catch 'type-error (make-component :render "not-a-proc"))

(check-report)
