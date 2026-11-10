;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

;; 鼠标滚轮滚动加速器：把滚轮事件换算成滚动行数。
;; 对齐上游 packages/tui/src/wheel-scroll.ts：
;;   - "auto" 模式下，单次滚轮（孤立事件）滚动 1 行；
;;     快速连续滚动时按事件间隔加速，最多 6 行；
;;   - 固定模式（正整数）始终按该行数滚动。
;;
;; 终端差异（对齐上游 terminalAcceleratesWheel）：
;; macOS 本地终端（未经过 SSH）的滚轮事件已被系统加速，
;; 每个事件只代表 1 行，因此不再做二次加速。
(define-library (liii tui wheel-scroll)
  (export make-wheel-scroll-accelerator wheel-scroll-accelerator?
    wheel-scroll-next! wheel-scroll-reset! wheel-scroll-lines
    wheel-scroll-set-lines!
  ) ;export
  (import (scheme base) (liii base) (liii error))
  (begin

    ;; 同一物理滚轮刻度内的突发事件间隔（约 4ms 一次）
    (define %burst-gap-ms 5)
    ;; 超过该间隔视为一次新的滚动手势
    (define %gesture-gap-ms 200)
    ;; 参考间隔：事件间隔等于该值时每次事件滚动 1 行
    (define %reference-gap-ms 100)
    ;; 自动加速下的单次最大滚动行数
    (define %max-auto-lines 6)
    ;; 初始时间戳：保证第一次事件被判定为新手势
    (define %initial-time -1000000000)

    (define-record-type <wheel-scroll-accelerator>
      (%make-wheel-scroll-accelerator lines last-time last-direction average-gap
        carry
      ) ;%make-wheel-scroll-accelerator
      wheel-scroll-accelerator?
      (lines %wheel-scroll-lines %wheel-scroll-lines-set!)
      (last-time %wheel-scroll-last-time %wheel-scroll-last-time-set!)
      (last-direction %wheel-scroll-last-direction %wheel-scroll-last-direction-set!)
      (average-gap %wheel-scroll-average-gap %wheel-scroll-average-gap-set!)
      (carry %wheel-scroll-carry %wheel-scroll-carry-set!)
    ) ;define-record-type

    ;; 合法的滚动行数模式：'auto 或正整数
    (define (%valid-lines? lines)
      (if (eq? lines 'auto) #t (and (integer? lines) (exact? lines) (> lines 0)))
    ) ;define

    (define (make-wheel-scroll-accelerator . args)
      (let ((lines 'auto))
        (let parse
          ((rest args))
          (cond ((null? rest) #t)
                ((eq? (car rest) :lines)
                 (when (null? (cdr rest))
                   (type-error "make-wheel-scroll-accelerator: missing value for :lines")
                 ) ;when
                 (set! lines (cadr rest))
                 (parse (cddr rest))
                ) ;
                (else (type-error "make-wheel-scroll-accelerator: invalid keyword arguments"))
          ) ;cond
        ) ;let
        (unless (%valid-lines? lines)
          (type-error "make-wheel-scroll-accelerator: :lines must be 'auto or a positive integer"
          ) ;type-error
        ) ;unless
        (%make-wheel-scroll-accelerator lines %initial-time 0 #f 0)
      ) ;let
    ) ;define

    (define (wheel-scroll-lines acc)
      (unless (wheel-scroll-accelerator? acc)
        (type-error "wheel-scroll-lines: expected a <wheel-scroll-accelerator>")
      ) ;unless
      (%wheel-scroll-lines acc)
    ) ;define

    (define (wheel-scroll-set-lines! acc lines)
      (unless (wheel-scroll-accelerator? acc)
        (type-error "wheel-scroll-set-lines!: expected a <wheel-scroll-accelerator>")
      ) ;unless
      (unless (%valid-lines? lines)
        (type-error "wheel-scroll-set-lines!: lines must be 'auto or a positive integer"
        ) ;type-error
      ) ;unless
      (%wheel-scroll-lines-set! acc lines)
      (wheel-scroll-reset! acc)
      lines
    ) ;define

    (define (wheel-scroll-reset! acc)
      (unless (wheel-scroll-accelerator? acc)
        (type-error "wheel-scroll-reset!: expected a <wheel-scroll-accelerator>")
      ) ;unless
      (%wheel-scroll-last-time-set! acc %initial-time)
      (%wheel-scroll-last-direction-set! acc 0)
      (%wheel-scroll-average-gap-set! acc #f)
      (%wheel-scroll-carry-set! acc 0)
      #t
    ) ;define

    ;; 返回本次滚轮事件应滚动的行数（正整数）
    ;; direction：-1 向上，1 向下；now-ms：事件发生时刻（毫秒）
    (define (wheel-scroll-next! acc direction now-ms)
      (unless (wheel-scroll-accelerator? acc)
        (type-error "wheel-scroll-next!: expected a <wheel-scroll-accelerator>")
      ) ;unless
      (unless (and (integer? direction) (exact? direction))
        (type-error "wheel-scroll-next!: direction must be an integer")
      ) ;unless
      (let* ((lines (wheel-scroll-lines acc))
             (gap (- now-ms (%wheel-scroll-last-time acc)))
             (same-gesture? (and (= direction (%wheel-scroll-last-direction acc)) (<= gap %gesture-gap-ms))
             ) ;same-gesture?
            ) ;
        (%wheel-scroll-last-time-set! acc now-ms)
        (%wheel-scroll-last-direction-set! acc direction)
        (cond ((not (eq? lines 'auto)) (max 1 lines))
              ((not same-gesture?)
               ;; 新手势：重置平均间隔与进位，滚动 1 行
               (%wheel-scroll-average-gap-set! acc #f)
               (%wheel-scroll-carry-set! acc 0)
               1
              ) ;
              ((< gap %burst-gap-ms)
               ;; 同一物理刻度的突发事件：不加速
               1
              ) ;
              (else
                (let* ((prev (%wheel-scroll-average-gap acc))
                       (avg (if prev (/ (+ prev gap) 2) gap))
                       (base (min %max-auto-lines (max 1 (/ %reference-gap-ms avg))))
                       (scaled (+ base (%wheel-scroll-carry acc)))
                       (whole (inexact->exact (floor scaled)))
                      ) ;
                  (%wheel-scroll-average-gap-set! acc avg)
                  (%wheel-scroll-carry-set! acc (- scaled whole))
                  (max 1 whole)
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
