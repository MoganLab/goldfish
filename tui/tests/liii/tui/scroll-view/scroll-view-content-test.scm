;; scroll-view-content
;; 获取滚动视口内部包含的内容组件。
;;
;; 语法
;; ----
;; (scroll-view-content sv)
;;
;; 参数
;; ----
;; sv : scroll-view?
;; 滚动视口实例。
;;
;; 返回值
;; ----
;; component?
;; 内部的内容组件。

(import (scheme base) (liii check) (liii tui component) (liii tui scroll-view))

(check-set-mode! 'report-failed)

(let* ((comp (make-text "content")) (sv (make-scroll-view comp)))
  (check (scroll-view-content sv) => comp)
) ;let*

(check-catch 'type-error (scroll-view-content "not-a-sv"))

(check-report)
