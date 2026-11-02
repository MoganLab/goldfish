;; scroll-view-set-content!
;; 替换滚动视口内部包含的内容组件，并使行缓存失效。
;;
;; 语法
;; ----
;; (scroll-view-set-content! sv content)
;;
;; 参数
;; ----
;; sv : scroll-view?
;; 滚动视口实例。
;;
;; content : component?
;; 新的内容组件。
;;
;; 返回值
;; ----
;; component?
;; 设置成功返回新的内容组件。

(import (scheme base) (liii check) (liii tui component) (liii tui scroll-view))

(check-set-mode! 'report-failed)

(let* ((c1 (make-text "c1")) (c2 (make-text "c2")) (sv (make-scroll-view c1)))
  (check (scroll-view-content sv) => c1)
  (check (scroll-view-set-content! sv c2) => c2)
  (check (scroll-view-content sv) => c2)
) ;let*

(let ((sv (make-scroll-view (make-text "x"))))
  (check-catch 'type-error (scroll-view-set-content! sv "not-a-component"))
) ;let

(check-report)
