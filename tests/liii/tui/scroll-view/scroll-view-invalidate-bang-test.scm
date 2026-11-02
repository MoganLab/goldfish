;; scroll-view-invalidate!
;; 清空视口内部渲染行与总行数的缓存，强制下一次渲染重新计算。
;;
;; 语法
;; ----
;; (scroll-view-invalidate! sv)
;;
;; 参数
;; ----
;; sv : scroll-view?
;; 滚动视口实例。
;;
;; 返回值
;; ----
;; #t
;; 操作成功返回 #t。

(import (scheme base) (liii check) (liii tui component) (liii tui scroll-view))

(check-set-mode! 'report-failed)

(let ((sv (make-scroll-view (make-text "test"))))
  (check-true (scroll-view-invalidate! sv))
) ;let

(check-catch 'type-error (scroll-view-invalidate! "not-a-sv"))

(check-report)
