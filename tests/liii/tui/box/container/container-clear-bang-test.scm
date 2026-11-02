;; container-clear!
;; 清空容器内的所有子组件。
;;
;; 语法
;; ----
;; (container-clear! container)
;;
;; 参数
;; ----
;; container : container?
;; 容器组件实例。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui component) (liii tui box))

(check-set-mode! 'report-failed)

(let ((c (make-container)))
  (container-add! c "alpha")
  (container-add! c "beta")
  (check (component-render c 80) => '("alpha" "beta"))
  (container-clear! c)
  (check (container-children c) => '())
  (check (component-render c 80) => '())
) ;let

(check-catch 'type-error (container-clear! "not-a-container"))

(check-report)
