(import (scheme base)
        (liii check)
        (liii list)
        (liii string)
        (liii tui)
        (liii tui mouse)
        (liii pi code modes interactive)
        (liii pi code modes interactive app)
        (liii pi code modes interactive views))

(check-set-mode! 'report-failed)

;; pi-app-handle-mouse-events!
;; 批量处理鼠标事件：连续滚轮事件合并为一次滚动（只滚动一次），
;; 点击事件照旧派发给卡片。
;;
;; 合并的意义：滚轮事件到达间隔（约 5~10ms）远小于单次滚动的渲染耗时，
;; 逐个处理会让输入队列不断积压，表现为“滑几下就越滚越慢”。

(define (%make-test-app)
  (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)
    :handler (lambda (a m) #t)
  ) ;make-pi-app
) ;define

(define (%fill app n)
  (let loop
    ((i 0))
    (when (< i n)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
) ;define

(define (wheel-up) (make-mouse-event 'wheel-up 'none 10 5))
(define (wheel-down) (make-mouse-event 'wheel-down 'none 10 5))

;; 1. 三个滚轮事件合并为一次滚动：偏移一次性移动 3 行
(let* ((app (%make-test-app))
       (sv (pi-app-scroll-view app))
      ) ;
  (%fill app 10)
  (pi-app-render! app)
  (check (scroll-view-follow? sv) => #t)
  (let ((before (scroll-view-offset sv)))
    (check (pi-app-handle-mouse-events! app (list (wheel-up) (wheel-up) (wheel-up))) => #t)
    (check (scroll-view-follow? sv) => #f)
    (check (= (- before (scroll-view-offset sv)) 3) => #t)
  ) ;let
) ;let*

;; 2. 上滚与下滚混合：净位移为 0（合并后只滚动一次）
(let* ((app (%make-test-app))
       (sv (pi-app-scroll-view app))
      ) ;
  (%fill app 10)
  (pi-app-render! app)
  (let ((before (scroll-view-offset sv)))
    (check (pi-app-handle-mouse-events! app (list (wheel-up) (wheel-down))) => #t)
    (check (= (scroll-view-offset sv) before) => #t)
  ) ;let
) ;let*

;; 3. 空事件列表：不消费任何事件
(let ((app (%make-test-app)))
  (check (pi-app-handle-mouse-events! app '()) => #f)
) ;let

;; 4. 单事件处理与批量处理行为一致
(let* ((app (%make-test-app))
       (sv (pi-app-scroll-view app))
      ) ;
  (%fill app 10)
  (pi-app-render! app)
  (let ((before (scroll-view-offset sv)))
    (check (pi-app-handle-mouse! app (wheel-up)) => #t)
    (check (= (- before (scroll-view-offset sv)) 1) => #t)
  ) ;let
) ;let*

;; 5. 非滚轮事件（按下/释放/拖动）不消费
(let ((app (%make-test-app)))
  (check (pi-app-handle-mouse-events! app
           (list (make-mouse-event 'press 'left 10 5)
             (make-mouse-event 'release 'left 10 5)
             (make-mouse-event 'drag 'left 10 5)
           )
         ) => #f)
) ;let

(check-report)
