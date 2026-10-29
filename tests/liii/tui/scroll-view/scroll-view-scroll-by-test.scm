(import (liii check)
        (liii tui component)
        (liii tui scroll-view))

(check-set-mode! 'report-failed)

;; 固定行数的内容组件
(define (make-lines-component lines)
  (make-component (lambda (comp width) lines))
) ;define

;; scroll-by! 复用上一帧的总量缓存：滚动本身不再重新渲染内容组件
(let ((renders 0))
  (let* ((comp (make-component
                  (lambda (self width)
                    (set! renders (+ renders 1))
                    '("l0" "l1" "l2" "l3" "l4")
                  ) ;lambda
                ))
         (sv (make-scroll-view comp))
        ) ;
    (check (scroll-view-render-lines sv 20 2) => '("l3" "l4"))
    (check (= renders 1) => #t)
    ;; 滚动不应再次渲染内容组件
    (check (scroll-view-scroll-by! sv -1) => 2)
    (check (= renders 1) => #t)
    (check (scroll-view-scroll-by! sv -1) => 1)
    (check (= renders 1) => #t)
    ;; 下一帧渲染才重新取内容
    (check (scroll-view-render-lines sv 20 2) => '("l1" "l2"))
    (check (= renders 2) => #t)
  ) ;let
) ;let

;; 未渲染过内容时滚动：按内容实际行数计算上限
(let ((sv (make-scroll-view (make-lines-component '("l0" "l1" "l2" "l3" "l4")))))
  (check (scroll-view-scroll-by! sv -1) => 0)
  (check (scroll-view-total-lines sv) => 5)
) ;let

;; 内容替换后总量缓存失效
(let ((sv (make-scroll-view (make-lines-component '("a" "b" "c")))))
  (scroll-view-render-lines sv 20 1)
  (check (scroll-view-total-lines sv) => 3)
  (scroll-view-set-content! sv (make-lines-component '("x" "y" "z" "w")))
  (check (scroll-view-total-lines sv) => 4)
) ;let

;; 显式失效：内容在组件内部变化时，调用 scroll-view-invalidate! 后重新统计
(let ((lines '("a" "b")))
  (let ((sv (make-scroll-view (make-component (lambda (self width) lines)))))
    (scroll-view-render-lines sv 20 2)
    (check (scroll-view-total-lines sv) => 2)
    (set! lines '("a" "b" "c" "d"))
    ;; 未失效前仍使用缓存
    (check (scroll-view-total-lines sv) => 2)
    (scroll-view-invalidate! sv)
    (check (scroll-view-total-lines sv) => 4)
  ) ;let
) ;let

;; 行内嵌换行仍按多行拆分（语义不变）
(let ((sv (make-scroll-view (make-lines-component '("a\nb" "c")))))
  (check (scroll-view-total-lines sv) => 3)
  (check (scroll-view-render-lines sv 20 5) => '("a" "b" "c"))
) ;let

;; 内容未变时拆分结果复用：多次渲染结果一致（不重复拆行）
(let ((sv (make-scroll-view (make-lines-component '("a\nb" "c" "d")))))
  (check (scroll-view-render-lines sv 20 5) => '("a" "b" "c" "d"))
  (check (scroll-view-render-lines sv 20 5) => '("a" "b" "c" "d"))
  (check (scroll-view-total-lines sv) => 4)
) ;let

;; 内容在组件内部变化：渲染时自动识别并重新拆分（缓存不陈旧）
(let ((lines '("a\nb" "c")))
  (let ((sv (make-scroll-view (make-component (lambda (self width) lines)))))
    (check (scroll-view-render-lines sv 20 5) => '("a" "b" "c"))
    (set! lines '("x" "y" "z"))
    (check (scroll-view-render-lines sv 20 5) => '("x" "y" "z"))
    (check (scroll-view-total-lines sv) => 3)
  ) ;let
) ;let

(check-report)
