(import (scheme base)
        (liii check)
        (liii list)
        (liii string)
        (liii time)
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

;; 6. 滚轮加速语义：同一手势内间隔 30ms 到达的连续滚轮事件，
;;    按 wheel-scroll 的速度规则应滚动 1 + 3 + 3 = 7 行（上游语义：
;;    孤立刻度 1 行，快速连打最多 6 行/次）。
;;    run.scm 的生产路径是「取首个事件 + 排空已到达事件 + 合并处理」，
;;    合并后各事件在 wheel-scroll-next! 中使用同一个处理时刻，
;;    事件到达间隔被记为 0（< 突发阈值 5ms），加速因此被抹平：
;;    同样的 3 个滚轮事件，合并处理只滚动 3 行。
;;    本用例断言合并路径与逐个处理路径的滚动量一致。
(let* ((seq-app (%make-test-app))
       (seq-sv (pi-app-scroll-view seq-app))
       (merged-app (%make-test-app))
       (merged-sv (pi-app-scroll-view merged-app))
      ) ;
  (%fill seq-app 10)
  (pi-app-render! seq-app)
  (%fill merged-app 10)
  (pi-app-render! merged-app)
  ;; 逐个处理：事件之间间隔 30ms，走完整的速度换算
  (let ((seq-before (scroll-view-offset seq-sv)))
    (pi-app-handle-mouse! seq-app (wheel-up))
    (sleep 0.03)
    (pi-app-handle-mouse! seq-app (wheel-up))
    (sleep 0.03)
    (pi-app-handle-mouse! seq-app (wheel-up))
    ;; 合并处理：生产路径的实际做法
    (let ((merged-before (scroll-view-offset merged-sv)))
      (pi-app-handle-mouse-events! merged-app
        (list (wheel-up) (wheel-up) (wheel-up))
      ) ;pi-app-handle-mouse-events!
      (check (= (- seq-before (scroll-view-offset seq-sv))
                (- merged-before (scroll-view-offset merged-sv))) => #t)
    ) ;let
  ) ;let
) ;let*

(check-report)
