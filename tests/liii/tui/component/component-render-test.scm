(import (liii check)
        (liii tui component))

;; component-render
;; 渲染组件内容为行列表。
;;
;; 语法
;; ----
;; (component-render comp width)
;;
;; 参数
;; ----
;; comp : component
;; 要渲染的组件对象。
;; width : integer
;; 可用的显示宽度（列数）。
;;
;; 返回值
;; ----
;; (list of string)
;; 渲染出的每一行文本所构成的字符串列表。
;;
;; 说明
;; ----
;; component-render 根据可用行宽 width 调用组件内部的 render 过程。
;; 宽度 width 必须为实数/整数；若 comp 不是合法组件或 width 类型非法，抛出 type-error。

(let ((c (make-component (lambda (comp w) (list "row1" "row2")))))
  (check (component-render c 80) => '("row1" "row2"))
  (check (component-render c 20) => '("row1" "row2")))

;; 检查单参数过程支持 (lambda (width) ...)
(let ((c (make-component (lambda (w) (list (number->string (* w 2)))))))
  (check (component-render c 50) => '("100")))

;; 错误处理
(check-catch 'type-error (component-render "not-a-component" 80))
(check-catch 'type-error (component-render 123 80))
(check-catch 'type-error (component-render (make-spacer 1) "not-a-width"))

(check-report)
