;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0
;;

;; pi 全屏模式滚轮滚动性能基准：
;; 测量「对话记录变长」与「单条消息变长」两种情形下，
;; 单次滚轮事件（滚动 + 整帧渲染）的耗时。
;;
;; 运行方式: bin/gf -I tools/pi bench/pi-scroll-perf.scm

(import (scheme base)
  (liii base)
  (liii string)
  (liii time)
  (liii date-time)
  (liii tui)
  (liii tui mouse)
  (liii pi code modes interactive)
  (liii pi code modes interactive app)
  (liii pi code modes interactive views)
) ;import

(define (%make-text n)
  (let loop ((i 0) (acc '()))
    (if (= i n)
      (string-join (reverse acc) "\n")
      (loop (+ i 1) (cons (string-append "第 " (number->string i) " 行：这是一段用于压测的消息内容") acc))
    ) ;if
  ) ;let
) ;define

(define (%make-app)
  (make-pi-app :terminal (make-virtual-terminal :width 80 :height 40)
    :handler (lambda (a m) #t)
  ) ;make-pi-app
) ;define

(define (%fill app n msg-lines)
  (let ((text (%make-text msg-lines)))
    (do ((i 0 (+ i 1)))
      ((= i n))
      (pi-app-add-user-message! app text)
      (pi-app-add-assistant-text! app text)
    ) ;do
  ) ;let
) ;define

(define (%now-ms)
  (current-monotonic-millis)
) ;define

;; 模拟一次滚轮事件：滚动 + 整帧重绘
(define (%wheel-step app)
  (pi-app-handle-mouse! app (make-mouse-event 'wheel-down 'none 5 5))
  (pi-app-render! app)
) ;define

(define (%bench title n-messages msg-lines iters)
  (let ((app (%make-app)))
    (%fill app n-messages msg-lines)
    (pi-app-render! app)
    (do ((i 0 (+ i 1))) ((= i 3)) (%wheel-step app)) ;warmup
    (let ((t0 (%now-ms)))
      (do ((i 0 (+ i 1))) ((= i iters)) (%wheel-step app))
      (let ((dt (- (%now-ms) t0)))
        (display "[")
        (display title)
        (display "] msgs=")
        (display n-messages)
        (display " lines/msg=")
        (display msg-lines)
        (display " iters=")
        (display iters)
        (display " total=")
        (display dt)
        (display "ms per-event=")
        (display (/ dt (exact->inexact iters)))
        (display "ms")
        (newline)
      ) ;let
    ) ;let
  ) ;let
) ;define

(display "=== 滚轮事件耗时（消息条数增长，每条 12 行）===")
(newline)
(%bench "12行x5条" 5 12 50)
(%bench "12行x20条" 20 12 50)
(%bench "12行x50条" 50 12 50)
(%bench "12行x100条" 100 12 50)

(display "")
(display "=== 滚轮事件耗时（单条消息变长，50 条）===")
(newline)
(%bench "40行x50条" 50 40 30)
(%bench "120行x50条" 50 120 20)
(%bench "400行x50条" 50 400 10)

;; 固定内容规模，测量不同滚动深度（offset）下单次滚轮事件的耗时
(define (%bench-depth title n-messages msg-lines depth iters)
  (let ((app (%make-app)))
    (%fill app n-messages msg-lines)
    (pi-app-render! app)
    (let ((sv (pi-app-scroll-view app)))
      (scroll-view-set-follow! sv #f)
      (scroll-view-set-offset! sv depth)
    ) ;let
    (do ((i 0 (+ i 1))) ((= i 3)) (pi-app-render! app))
    (let ((t0 (%now-ms)))
      (do ((i 0 (+ i 1))) ((= i iters))
        (let ((sv (pi-app-scroll-view app)))
          (scroll-view-set-follow! sv #f)
          (scroll-view-set-offset! sv (+ depth i))
        ) ;let
        (pi-app-render! app)
      ) ;do
      (let ((dt (- (%now-ms) t0)))
        (display "[")
        (display title)
        (display "] depth=")
        (display depth)
        (display " iters=")
        (display iters)
        (display " total=")
        (display dt)
        (display "ms per-frame=")
        (display (/ dt (exact->inexact iters)))
        (display "ms")
        (newline)
      ) ;let
    ) ;let
  ) ;let
) ;define

(display "")
(display "=== 单帧渲染耗时（内容固定 100条x40行，滚动深度递增）===")
(newline)
(%bench-depth "depth=0" 100 40 0 30)
(%bench-depth "depth=1000" 100 40 1000 30)
(%bench-depth "depth=3000" 100 40 3000 30)
(%bench-depth "depth=6000" 100 40 6000 30)
