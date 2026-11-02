;; make-select-list
;; 创建一个带键盘导航、滚动指示和回车/取消回调的选择列表组件。
;;
;; 语法
;; ----
;; (make-select-list items max-visible [:on-select on-select] [:on-cancel on-cancel])
;;
;; 参数
;; ----
;; items : list?
;; 候选列表项，每项为形如 ((value . v) (label . l) (description . d)) 的 alist。
;;
;; max-visible : integer?
;; 列表中同时可见的最大行数。
;;
;; on-select : procedure? (可选)
;; 用户选中项并按下 Enter 时的回调函数，接收选中的 item 作为参数。
;;
;; on-cancel : procedure? (可选)
;; 用户按下 Escape 或 Ctrl+C 取消时的无参回调函数。
;;
;; 返回值
;; ----
;; component?
;; 包装为 component 的选择列表实例。

(import (scheme base) (liii check) (liii tui component) (liii tui select-list))

(check-set-mode! 'report-failed)

(define (item value description)
  (list (cons 'value value) (cons 'label value) (cons 'description description))
) ;define

(let* ((items (list (item "a" "first") (item "b" "second")))
       (lst (make-select-list items 5))
      ) ;
  (check-true (component? lst))
  (check-true (select-list? lst))
) ;let*

(check-report)
