;; container-add!
;; 向容器末尾追加一个子组件，字符串自动包装为 make-text。
;;
;; 语法
;; ----
;; (container-add! container child)
;;
;; 参数
;; ----
;; container : container?
;; 容器组件实例。
;;
;; child : any
;; 待追加的子组件或字符串。
;;
;; 返回值
;; ----
;; component?
;; 返回被添加的子组件对象。

(import (scheme base) (liii check) (liii tui component) (liii tui box))

(check-set-mode! 'report-failed)

(let ((c (make-container)))
  (container-add! c (make-text "line 1"))
  (check (= (length (container-children c)) 1) => #t)
  (check (component-render c 80) => '("line 1"))

  ;; 字符串自动包装为 text component
  (container-add! c "line 2")
  (check (component-render c 80) => '("line 1" "line 2"))
) ;let

(check-catch 'type-error (container-add! "not-a-container" "ok"))
(check-catch 'type-error (container-add! (make-container) 123))

(check-report)
