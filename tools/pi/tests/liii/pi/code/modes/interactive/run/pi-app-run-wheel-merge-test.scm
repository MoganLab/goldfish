(import (scheme base)
        (liii check)
        (liii list)
        (liii string)
        (liii tui)
        (liii tui mouse)
        (liii pi code modes interactive)
        (liii pi code modes interactive app)
        (liii pi code modes interactive run))

(check-set-mode! 'report)

;; 主循环合并连续滚轮事件
;;
;; 滚轮事件到达间隔（约 5~10ms）远小于单次滚动的渲染耗时，
;; 逐个处理会让输入队列不断积压，表现为“滑几下就越滚越慢”。
;; 主循环应一次取走已到达的滚轮事件、合并为一次滚动、只渲染一帧。
;;
;; 断言：
;;   1. 三个滚轮事件被合并：视口一次性上移 3 行
;;   2. 只产生一帧重绘（帧数 = 初始帧 1 + 合并帧 1）

(define (%count-frames raw)
  (length (filter (lambda (chunk) (string-contains? chunk "\x1b;[?2026h")) raw))
) ;define

(let* ((vterm (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
       (sv (pi-app-scroll-view app))
      ) ;
  (let loop
    ((i 0))
    (when (< i 10)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-render! app)
  (let ((before (scroll-view-offset sv)))
    ;; 三个滚轮事件连发，随后 ctrl-d 退出
    (virtual-terminal-push-key! vterm (make-mouse-event 'wheel-up 'none 10 5))
    (virtual-terminal-push-key! vterm (make-mouse-event 'wheel-up 'none 10 5))
    (virtual-terminal-push-key! vterm (make-mouse-event 'wheel-up 'none 10 5))
    (virtual-terminal-push-key! vterm 'ctrl-d)
    (check (pi-app-run app) => #t)
    ;; 合并后一次性上移 3 行
    (check (scroll-view-follow? sv) => #f)
    (check (= (- before (scroll-view-offset sv)) 3) => #t)
    ;; 只重绘一帧（初始帧由 pi-app-render! 产生，合并帧 1 帧）
    (check (%count-frames (virtual-terminal-raw vterm)) => 2)
  ) ;let
) ;let*

(check-report)
