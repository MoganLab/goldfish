(import (liii check)
        (liii tui component))

;; make-spacer
;; 创建指定高度的空白占位组件。
;;
;; 语法
;; ----
;; (make-spacer [height])
;;
;; 参数
;; ----
;; height : integer, 可选
;; 占位的行数，默认为 1。必须为非负整数。
;;
;; 返回值
;; ----
;; component
;; 空白占位组件实例。
;;
;; 说明
;; ----
;; 渲染时返回由 height 个空字符串 "" 组成的行列表。
;; 若 height 类型非法或为负数，抛出相应错误。

;; 默认高度 1
(let ((c (make-spacer)))
  (check (component-render c 80) => '("")))

;; 指定高度
(let ((c (make-spacer 3)))
  (check (component-render c 80) => '("" "" ""))
  (check (component-render c 20) => '("" "" "")))

;; 高度为 0
(let ((c (make-spacer 0)))
  (check (component-render c 80) => '()))

;; 非法高度
(check-catch 'type-error (make-spacer "1"))
(check-catch 'value-error (make-spacer -1))

(check-report)
