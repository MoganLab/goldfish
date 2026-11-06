;; make-container
;; 创建动态子组件容器，支持动态追加与清空子组件。
;;
;; 语法
;; ----
;; (make-container)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; component?
;; 容器组件实例。

(import (scheme base) (liii check) (liii tui component) (liii tui box))

(check-set-mode! 'report-failed)

(let ((c (make-container)))
  (check-true (component? c))
  (check-true (container? c))
  (check (container-children c) => '())
  (check (component-render c 80) => '())
) ;let

(check-report)
