;; pi-app-handle-mouse-events!
;; 批量处理鼠标事件队列：连续滚轮事件合并为一次滚动，点击事件派发给卡片。
;;
;; 语法
;; ----
;; (pi-app-handle-mouse-events! app events)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; events : list of mouse-event?
;; 待处理的鼠标事件列表。
;;
;; 返回值
;; ------
;; boolean
;; 处理过程中是否有事件导致界面状态变更需要重绘。
;;
;; 说明
;; ----
;; 批量处理滚轮事件可有效避免事件积压与卡顿。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii time)
  (liii tui)
  (liii tui mouse)
  (liii pi code modes interactive)
  (liii pi code modes interactive app)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

(define (%make-test-app)
  (make-pi-app
    :terminal (make-virtual-terminal :width 80 :height 24)
    :handler  (lambda (a m) #t)
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

(define (wheel-up)
  (make-mouse-event 'wheel-up 'none 10 5)
) ;define

(define (wheel-down)
  (make-mouse-event 'wheel-down 'none 10 5)
) ;define

;; 1. 三个滚轮事件合并为一次滚动：偏移一次性移动 3 行
(let* ((app (%make-test-app)) (sv (pi-app-scroll-view app)))
  (%fill app 10)
  (pi-app-render! app)
  (check (scroll-view-follow? sv) => #t)
  (let ((before (scroll-view-offset sv)))
    (check (pi-app-handle-mouse-events! app (list (wheel-up) (wheel-up) (wheel-up)))
      =>
      #t
    ) ;check
    (check (scroll-view-follow? sv) => #f)
    (check (= (- before (scroll-view-offset sv)) 3) => #t)
  ) ;let
) ;let*

;; 2. 上滚与下滚混合：净位移为 0（合并后只滚动一次）
(let* ((app (%make-test-app)) (sv (pi-app-scroll-view app)))
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
(let* ((app (%make-test-app)) (sv (pi-app-scroll-view app)))
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
           ) ;list
         ) ;pi-app-handle-mouse-events!
    =>
    #f
  ) ;check
) ;let

;; 6. 滚轮加速语义（Issue #1382）：同一手势内间隔 30ms 到达的连续滚轮事件，
;;    按 wheel-scroll 的速度规则应滚动 1 + 3 + 3 = 7 行（孤立刻度 1 行，
;;    间隔 30ms 时每事件 3 行）。
;;    合并处理必须使用各事件自带的到达时刻，而非统一的处理时刻：
;;    否则批内间隔被记为 0（小于突发阈值 5ms），加速被抹平只剩 3 行。
;;    本用例对两条路径提供完全相同的到达时刻序列（0/30/60ms），
;;    断言合并路径与逐个处理路径的滚动量一致。

(define (wheel-up-at t)
  (make-mouse-event 'wheel-up 'none 10 5 #f #f #f t)
) ;define

(let* ((seq-app (%make-test-app))
       (seq-sv (pi-app-scroll-view seq-app))
       (merged-app (%make-test-app))
       (merged-sv (pi-app-scroll-view merged-app))
      ) ;
  (%fill seq-app 10)
  (pi-app-render! seq-app)
  (%fill merged-app 10)
  (pi-app-render! merged-app)
  (let ((seq-before (scroll-view-offset seq-sv))
        (merged-before (scroll-view-offset merged-sv))
       ) ;
    ;; 逐个处理：每个事件携带自己的到达时刻
    (for-each (lambda (t) (pi-app-handle-mouse! seq-app (wheel-up-at t)))
      '(0 30 60)
    ) ;for-each
    ;; 合并处理：生产路径，同样的到达时刻序列
    (pi-app-handle-mouse-events! merged-app
      (list (wheel-up-at 0) (wheel-up-at 30) (wheel-up-at 60))
    ) ;pi-app-handle-mouse-events!
    ;; 两条路径都应滚动 7 行，且完全一致
    (check (- seq-before (scroll-view-offset seq-sv)) => 7)
    (check (- merged-before (scroll-view-offset merged-sv)) => 7)
    (check (= (- seq-before (scroll-view-offset seq-sv))
             (- merged-before (scroll-view-offset merged-sv))
           ) ;=
      =>
      #t
    ) ;check
  ) ;let
) ;let*

;; 7. 鼠标事件时间戳：make-mouse-event 的可选 time 参数存取，
;;    未提供时为 #f（未知，回退到处理时刻）
(let ((ev (make-mouse-event 'wheel-up 'none 10 5 #f #f #f 12345)) (plain (wheel-up)))
  (check (mouse-event-time ev) => 12345)
  (check (mouse-event-time plain) => #f)
  ;; 时间戳不影响事件其他字段
  (check (mouse-event-type ev) => 'wheel-up)
  (check (mouse-event-x ev) => 10)
  (check (mouse-event-y ev) => 5)
) ;let

;; 8. 虚拟终端 push-key! 为无时间戳的鼠标事件补打到达时刻
(let ((vt (make-virtual-terminal 40 20)))
  (virtual-terminal-push-key! vt (wheel-up))
  (let ((ev (terminal-poll-key vt)))
    (check-true (mouse-event? ev))
    (check-true (number? (mouse-event-time ev)))
  ) ;let
  ;; 已带时间戳的事件不被改写
  (virtual-terminal-push-key! vt (wheel-up-at 777))
  (check (mouse-event-time (terminal-poll-key vt)) => 777)
) ;let

(check-report)
