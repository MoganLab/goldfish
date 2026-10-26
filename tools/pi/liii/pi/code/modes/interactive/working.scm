;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 模仿 pi 的 Working 流式特效：
;; 任务执行期间，编辑器上边框嵌入盲文点阵 spinner + "Working" 提示，
;; 80ms 一帧循环滚动，全部使用 pi 的紫色主题色渲染
(define-library (liii pi code modes interactive working)
  (export make-working-indicator working-indicator? working-frame
    working-frame-index working-advance! working-reset! working-last-tick
    working-last-tick-set! working-border-line %working-frame-interval-ms
  ) ;export
  (import (scheme base) (liii string) (liii unicode) (liii tui text-width))
  (begin

    ;; 盲文点阵帧序列与动画节拍，与 pi 的 Loader 默认值一致
    (define %working-frames
      (list "⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
    ) ;define
    (define %working-frame-interval-ms 80)

    ;; pi 的 thinking 边框紫色（tmux 实测 RGB 151,118,229）；
    ;; 现由调用方按当前 thinking level 注入颜色，与编辑器边框保持一致
    (define %ansi-reset "\x1b;[0m")

    ;; Working 指示器状态：
    ;; frame-index: 当前帧下标（0..9）
    ;; last-tick:   上次动画节拍的单调毫秒时间戳，#f 表示尚未开始
    (define-record-type <working-indicator>
      (%make-working-indicator frame-index last-tick)
      working-indicator?
      (frame-index %working-frame-index %working-frame-index-set!)
      (last-tick working-last-tick working-last-tick-set!)
    ) ;define-record-type

    (define (make-working-indicator)
      (%make-working-indicator 0 #f)
    ) ;define

    (define (working-frame ind)
      (list-ref %working-frames (%working-frame-index ind))
    ) ;define

    (define (working-frame-index ind)
      (%working-frame-index ind)
    ) ;define

    ;; 推进到下一帧，循环滚动
    (define (working-advance! ind)
      (%working-frame-index-set! ind
        (modulo (+ (%working-frame-index ind) 1) (length %working-frames))
      ) ;%working-frame-index-set!
    ) ;define

    ;; 复位到初始帧与未开始状态（idle 时调用，下次 busy 从 ⠋ 重新开始）
    (define (working-reset! ind)
      (%working-frame-index-set! ind 0)
      (working-last-tick-set! ind #f)
    ) ;define

    ;; 渲染嵌入编辑器上边框的 Working 行：
    ;; "── ⠋ Working " 前缀 + 横线填满整行，统一用 color 包裹
    (define (working-border-line ind width color)
      (let* ((prefix (string-append "── " (working-frame ind) " Working "))
             (fill (max 0 (- width (display-width prefix))))
             (dashes (utf8-make-string fill #\─))
            ) ;
        (string-append color prefix dashes %ansi-reset)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
