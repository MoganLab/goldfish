(import (liii check)
        (liii tui component)
        (liii tui scroll-view))

(check-set-mode! 'report-failed)

;; 固定行数的内容组件
(define (make-lines-component lines)
  (make-component (lambda (comp width) lines))
) ;define

;; make-scroll-view 与属性访问
(let ((sv (make-scroll-view (make-lines-component '("l0")))))
  (check (scroll-view? sv) => #t)
  (check (scroll-view-follow? sv) => #t)
  (check (scroll-view-offset sv) => 0)
  (check-catch 'type-error (make-scroll-view "not-a-component"))
) ;let

;; follow 模式：视口贴底展示最新内容
(let ((sv (make-scroll-view (make-lines-component '("l0" "l1" "l2" "l3" "l4")))))
  (check (scroll-view-render-lines sv 20 2) => '("l3" "l4"))
  (check (scroll-view-total-lines sv) => 5)
  (check (scroll-view-max-offset sv) => 3)
) ;let

;; 主动滚动后退出 follow，位置保持
(let ((sv (make-scroll-view (make-lines-component '("l0" "l1" "l2" "l3" "l4")))))
  (scroll-view-render-lines sv 20 2)
  (check (scroll-view-scroll-by! sv -1) => 2)
  (check (scroll-view-follow? sv) => #f)
  (check (scroll-view-render-lines sv 20 2) => '("l2" "l3"))
  ;; 再向下滚回末尾时恢复 follow
  (check (scroll-view-scroll-by! sv 1) => 3)
  (check (scroll-view-follow? sv) => #t)
  (check (scroll-view-render-lines sv 20 2) => '("l3" "l4"))
) ;let

;; 翻页
(let ((sv (make-scroll-view (make-lines-component '("l0" "l1" "l2" "l3" "l4" "l5")))))
  (scroll-view-render-lines sv 20 2)
  (check (scroll-view-page-up! sv) => 2)
  (check (scroll-view-render-lines sv 20 2) => '("l2" "l3"))
  (check (scroll-view-page-down! sv) => 4)
  (check (scroll-view-render-lines sv 20 2) => '("l4" "l5"))
) ;let

;; 跳到顶部/底部
(let ((sv (make-scroll-view (make-lines-component '("l0" "l1" "l2" "l3" "l4")))))
  (scroll-view-render-lines sv 20 2)
  (check (scroll-view-to-top! sv) => 0)
  (check (scroll-view-follow? sv) => #f)
  (check (scroll-view-render-lines sv 20 2) => '("l0" "l1"))
  (check (scroll-view-to-bottom! sv) => 3)
  (check (scroll-view-follow? sv) => #t)
  (check (scroll-view-render-lines sv 20 2) => '("l3" "l4"))
) ;let

;; 滚动边界钳制
(let ((sv (make-scroll-view (make-lines-component '("l0" "l1")))))
  (scroll-view-render-lines sv 20 5)
  (check (scroll-view-scroll-by! sv -10) => 0)
  (check (scroll-view-scroll-by! sv 10) => 0)
  (check (scroll-view-render-lines sv 20 5) => '("l0" "l1"))
) ;let

;; 内容替换
(let* ((comp (make-lines-component '("a" "b")))
       (sv (make-scroll-view comp)))
  (scroll-view-render-lines sv 20 1)
  (check (scroll-view-render-lines sv 20 1) => '("b"))
  (scroll-view-set-content! sv (make-lines-component '("x")))
  (check (scroll-view-render-lines sv 20 1) => '("x"))
) ;let

;; 类型检查
(let ((sv (make-scroll-view (make-lines-component '("a")))))
  (check-catch 'type-error (scroll-view-render-lines 123 20 2))
  (check-catch 'type-error (scroll-view-scroll-by! sv "x"))
  (check-catch 'type-error (scroll-view-set-offset! sv "x"))
) ;let

(check-report)
