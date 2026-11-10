;; make-scroll-view
;; 创建包含内容组件的可滚动视口实例。
;;
;; 语法
;; ----
;; (make-scroll-view content)
;;
;; 参数
;; ----
;; content : component?
;; 被视口包裹的内容组件。
;;
;; 返回值
;; ----
;; scroll-view?
;; 滚动视口实例。

(import (scheme base) (liii check) (liii tui component) (liii tui scroll-view))

(check-set-mode! 'report-failed)

(let* ((comp (make-text "test")) (sv (make-scroll-view comp)))
  (check-true (scroll-view? sv))
  (check (scroll-view-content sv) => comp)
) ;let*

(check-catch 'type-error (make-scroll-view "not-a-component"))

(check-report)
