(import (scheme base)
        (liii check)
        (liii tui mouse)
        (liii tui component)
        (liii tui box))

(check-set-mode! 'report-failed)

;; 辅助：构造记录命中事件的组件，渲染高度由参数指定
(define (clickable-probe lines-rendered)
  (let ((events '()))
    (cons (make-component
            :render (lambda (self w) (make-list lines-rendered "line"))
            :handle-mouse (lambda (self ev)
                            (set! events (cons ev events))
                            #t))
      (lambda () (reverse events)))
  ) ;cons
) ;define

;; 1. container 渲染时登记子组件高度；点击按高度区间命中并以局部坐标派发
(let* ((p1 (clickable-probe 2))    ; 占 2 行：逻辑行 0-1
       (p2 (clickable-probe 3))    ; 占 3 行：逻辑行 2-4
       (c1 (car p1)) (c2 (car p2))
       (c (make-container))
      ) ;let*
  (container-add! c c1)
  (container-add! c c2)
  (component-render c 40)
  ;; 点击逻辑行 0：命中第一个子组件，局部 y=0
  (check (component-handle-mouse! c (make-mouse-event 'click 'left 5 0)) => #t)
  (check (map mouse-event-y ((cdr p1))) => '(0))
  (check (map mouse-event-y ((cdr p2))) => '())
  ;; 点击逻辑行 2：命中第二个子组件，局部 y=0（2 - 2）
  (check (component-handle-mouse! c (make-mouse-event 'click 'left 5 2)) => #t)
  (check (map mouse-event-y ((cdr p2))) => '(0))
  ;; 点击逻辑行 4：命中第二个子组件，局部 y=2
  (check (component-handle-mouse! c (make-mouse-event 'click 'left 5 4)) => #t)
  (check (map mouse-event-y ((cdr p2))) => '(0 2))
  ;; 子组件 x 坐标保持不变（vstack 纵向布局不换算 x）
  (check (map mouse-event-x ((cdr p2))) => '(5 5))
) ;let*

;; 2. 命中的子组件未处理鼠标时，container 返回未处理（不吞事件）
(let* ((plain (make-component :render (lambda (self w) '("a" "b"))))
       (c (make-container)))
  (container-add! c plain)
  (component-render c 40)
  (check (component-handle-mouse! c (make-mouse-event 'click 'left 0 1)) => #f)
) ;let*

;; 3. 未渲染过的 container 派发鼠标（无布局缓存）不崩溃、不命中
(let ((c (make-container)))
  (container-add! c "text")
  (check (component-handle-mouse! c (make-mouse-event 'click 'left 0 0)) => #f)
) ;let

;; 4. 渲染宽度变化后布局缓存失效，重新按新高度命中
(let* ((p (clickable-probe 2))
       (c (make-container))
       (wide (make-text "0123456789"))
      ) ;let*
  (container-add! c (car p))
  (container-add! c wide)
  (component-render c 20)  ; wide 不折行：布局 行 0-1=probe, 行 2-12=wide
  (check (component-handle-mouse! c (make-mouse-event 'click 'left 0 0)) => #t)
  (check (map mouse-event-y ((cdr p))) => '(0))
  ;; 点击行 2 落在 wide 上，probe 不命中
  (check (component-handle-mouse! c (make-mouse-event 'click 'left 0 2)) => #f)
  (check (map mouse-event-y ((cdr p))) => '(0))
) ;let*

;; 5. 空容器返回未处理
(let ((c (make-container)))
  (check (component-handle-mouse! c (make-mouse-event 'click 'left 0 0)) => #f)
) ;let

;; 6. vstack 同样支持按位置派发（spacing 计入布局偏移）
(let* ((p1 (clickable-probe 1))
       (p2 (clickable-probe 1))
       (v (make-vstack (list (car p1) (car p2)) :spacing 1)))
  (component-render v 40)
  ;; 布局：行 0 = child1，行 1 = spacing，行 2 = child2
  (check (component-handle-mouse! v (make-mouse-event 'click 'left 0 2)) => #t)
  (check (map mouse-event-y ((cdr p2))) => '(0))
  ;; spacing 行不属于任何子组件：未处理
  (check (component-handle-mouse! v (make-mouse-event 'click 'left 0 1)) => #f)
) ;let*

;; 7. 嵌套容器：外层按子容器高度命中，内层再按局部坐标二次派发
(let* ((inner-p (clickable-probe 2))
       (inner (make-container))
       (outer (make-container)))
  (container-add! inner (car inner-p))
  (container-add! outer (make-text "head"))
  (container-add! outer inner)
  (component-render inner 40)
  (component-render outer 40)
  ;; outer 布局：行 0 = "head"，行 1-2 = inner；点击行 2 → inner 局部行 1 → probe 局部行 1
  (check (component-handle-mouse! outer (make-mouse-event 'click 'left 0 2)) => #t)
  (check (map mouse-event-y ((cdr inner-p))) => '(1))
) ;let*

(check-report)
