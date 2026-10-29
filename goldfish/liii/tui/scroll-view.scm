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

;; 滚动视口：在全屏场景下承载可滚动内容（如对话记录）。
;; 对齐上游 packages/tui/src/components/scroll-view.ts 的核心语义：
;;   - follow 模式下视口始终贴着内容末尾，新内容到达时自动跟随；
;;   - 用户主动滚动后退出 follow，滚动位置保持不变；
;;   - 滚动到末尾时重新进入 follow。
(define-library (liii tui scroll-view)
  (export make-scroll-view scroll-view? scroll-view-content
    scroll-view-set-content! scroll-view-offset scroll-view-set-offset!
    scroll-view-follow? scroll-view-set-follow! scroll-view-render-lines
    scroll-view-scroll-by! scroll-view-page-up! scroll-view-page-down!
    scroll-view-to-top! scroll-view-to-bottom! scroll-view-total-lines
    scroll-view-max-offset scroll-view-height
  ) ;export
  (import (scheme base)
    (liii base)
    (liii error)
    (liii list)
    (liii string)
    (liii tui component)
  ) ;import
  (begin

    (define-record-type <scroll-view>
      (%make-scroll-view content offset follow? width height)
      scroll-view?
      (content %scroll-view-content %scroll-view-content-set!)
      (offset %scroll-view-offset %scroll-view-offset-set!)
      (follow? %scroll-view-follow? %scroll-view-follow?-set!)
      ;; 最近一次渲染的视口尺寸：滚动操作据此换算页大小与边界
      (width %scroll-view-width %scroll-view-width-set!)
      (height %scroll-view-height %scroll-view-height-set!)
    ) ;define-record-type

    (define (%check-scroll-view sv caller)
      (unless (scroll-view? sv)
        (type-error (string-append caller ": expected a <scroll-view>, got " (object->string sv))
        ) ;type-error
      ) ;unless
    ) ;define

    (define (make-scroll-view content)
      (unless (component? content)
        (type-error (string-append "make-scroll-view: expected component, got "
                      (object->string content)
                    ) ;string-append
        ) ;type-error
      ) ;unless
      (%make-scroll-view content 0 #t 80 24)
    ) ;define

    (define (scroll-view-content sv)
      (%check-scroll-view sv "scroll-view-content")
      (%scroll-view-content sv)
    ) ;define

    (define (scroll-view-set-content! sv content)
      (%check-scroll-view sv "scroll-view-set-content!")
      (unless (component? content)
        (type-error (string-append "scroll-view-set-content!: expected component, got "
                      (object->string content)
                    ) ;string-append
        ) ;type-error
      ) ;unless
      (%scroll-view-content-set! sv content)
      #t
    ) ;define

    (define (scroll-view-offset sv)
      (%check-scroll-view sv "scroll-view-offset")
      (%scroll-view-offset sv)
    ) ;define

    (define (scroll-view-set-offset! sv offset)
      (%check-scroll-view sv "scroll-view-set-offset!")
      (unless (and (integer? offset) (exact? offset))
        (type-error (string-append "scroll-view-set-offset!: expected integer, got "
                      (object->string offset)
                    ) ;string-append
        ) ;type-error
      ) ;unless
      (%scroll-view-offset-set! sv (max 0 offset))
      #t
    ) ;define

    (define (scroll-view-follow? sv)
      (%check-scroll-view sv "scroll-view-follow?")
      (%scroll-view-follow? sv)
    ) ;define

    (define (scroll-view-set-follow! sv flag)
      (%check-scroll-view sv "scroll-view-set-follow!")
      (%scroll-view-follow?-set! sv (if flag #t #f))
      #t
    ) ;define

    ;; 内容行：组件渲染结果，拆分内嵌换行，保证一行即一个终端行
    (define (%scroll-content-lines sv width)
      (let ((raw (component-render (scroll-view-content sv) width)))
        (append-map (lambda (item)
                      (if (string-contains? item "\n") (string-split item "\n") (list item))
                    ) ;lambda
          raw
        ) ;append-map
      ) ;let
    ) ;define

    (define (scroll-view-total-lines sv)
      (%check-scroll-view sv "scroll-view-total-lines")
      (length (%scroll-content-lines sv (%scroll-view-width sv)))
    ) ;define

    ;; 最大滚动偏移：内容总行数减去视口高度，最小为 0
    ;; 最近一次渲染的视口高度（行）：供鼠标命中判断是否落在视口内
    (define (scroll-view-height sv)
      (%check-scroll-view sv "scroll-view-height")
      (%scroll-view-height sv)
    ) ;define

    (define (scroll-view-max-offset sv)
      (%check-scroll-view sv "scroll-view-max-offset")
      (max 0 (- (scroll-view-total-lines sv) (%scroll-view-height sv)))
    ) ;define

    ;; 渲染视口内的可见行；follow 模式下自动贴底
    (define (scroll-view-render-lines sv width height)
      (%check-scroll-view sv "scroll-view-render-lines")
      (unless (and (real? width) (real? height))
        (type-error "scroll-view-render-lines: width and height must be numbers")
      ) ;unless
      (let* ((w (inexact->exact (round width)))
             (h (max 0 (inexact->exact (round height))))
             (lines (%scroll-content-lines sv w))
             (total (length lines))
             (max-offset (max 0 (- total h)))
             (offset
               (if (scroll-view-follow? sv)
                 max-offset
                 (max 0 (min (scroll-view-offset sv) max-offset))
               ) ;if
             ) ;offset
            ) ;
        (%scroll-view-width-set! sv w)
        (%scroll-view-height-set! sv h)
        ;; 始终同步偏移量：follow 模式下也要记录实际贴底位置，
        ;; 否则后续翻页会相对过期的偏移量计算
        (scroll-view-set-offset! sv offset)
        (let loop
          ((rest lines) (i 0) (acc '()))
          (cond
           ((or (>= (length acc) h) (null? rest)) (reverse acc))
           ((>= i offset) (loop (cdr rest) (+ i 1) (cons (car rest) acc)))
           (else (loop (cdr rest) (+ i 1) acc))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    ;; 相对滚动 delta 行：正数向下，负数向上；滚动到末尾时恢复 follow
    (define (scroll-view-scroll-by! sv delta)
      (%check-scroll-view sv "scroll-view-scroll-by!")
      (unless (and (integer? delta) (exact? delta))
        (type-error (string-append "scroll-view-scroll-by!: expected integer, got "
                      (object->string delta)
                    ) ;string-append
        ) ;type-error
      ) ;unless
      (let* ((max-offset (scroll-view-max-offset sv))
             (offset
               (max 0 (min (+ (scroll-view-offset sv) delta) max-offset))
             ) ;offset
            ) ;
        (scroll-view-set-offset! sv offset)
        (scroll-view-set-follow! sv (>= offset max-offset))
        offset
      ) ;let*
    ) ;define

    (define (scroll-view-page-up! sv)
      (%check-scroll-view sv "scroll-view-page-up!")
      (scroll-view-scroll-by! sv (- (%scroll-view-height sv)))
    ) ;define

    (define (scroll-view-page-down! sv)
      (%check-scroll-view sv "scroll-view-page-down!")
      (scroll-view-scroll-by! sv (%scroll-view-height sv))
    ) ;define

    (define (scroll-view-to-top! sv)
      (%check-scroll-view sv "scroll-view-to-top!")
      (scroll-view-set-offset! sv 0)
      (scroll-view-set-follow! sv #f)
      0
    ) ;define

    (define (scroll-view-to-bottom! sv)
      (%check-scroll-view sv "scroll-view-to-bottom!")
      (let ((max-offset (scroll-view-max-offset sv)))
        (scroll-view-set-offset! sv max-offset)
        (scroll-view-set-follow! sv #t)
        max-offset
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
