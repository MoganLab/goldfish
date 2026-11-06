;; container-children
;; 获取容器当前持有的所有子组件列表。
;;
;; 语法
;; ----
;; (container-children container)
;;
;; 参数
;; ----
;; container : container?
;; 容器组件实例。
;;
;; 返回值
;; ----
;; list?
;; 子组件列表。

(import (scheme base) (liii check) (liii tui box))

(check-set-mode! 'report-failed)

(let ((c (make-container)))
  (check (container-children c) => '())
  (container-add! c "a")
  (container-add! c "b")
  (check (= (length (container-children c)) 2) => #t)
) ;let

(check-catch 'type-error (container-children "not-a-container"))

(check-report)
