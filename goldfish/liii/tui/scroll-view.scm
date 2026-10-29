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
    scroll-view-max-offset scroll-view-height scroll-view-invalidate!
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
      (%make-scroll-view content offset follow? width height total rendered split)
      scroll-view?
      (content %scroll-view-content %scroll-view-content-set!)
      (offset %scroll-view-offset %scroll-view-offset-set!)
      (follow? %scroll-view-follow? %scroll-view-follow?-set!)
      ;; 最近一次渲染的视口尺寸：滚动操作据此换算页大小与边界
      (width %scroll-view-width %scroll-view-width-set!)
      (height %scroll-view-height %scroll-view-height-set!)
      ;; 最近一次渲染的内容总行数（缓存）：
      ;; 滚动时据此换算上界，避免为求上界再渲染一遍全部内容
      (total %scroll-view-total %scroll-view-total-set!)
      ;; 最近一次的内容渲染结果与拆分结果（缓存）：
      ;; 内容未变时复用，避免逐行扫描换行
      (rendered %scroll-view-rendered %scroll-view-rendered-set!)
      (split %scroll-view-split %scroll-view-split-set!)
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
      (%make-scroll-view content 0 #t 80 24 #f #f #f)
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
      ;; 内容换了：上一帧的行数缓存不再适用
      (%scroll-view-total-set! sv #f)
      #t
    ) ;define

    ;; 内容行：组件渲染结果，拆分内嵌换行，保证一行即一个终端行。
    ;;
    ;; 拆分要逐行判断是否含换行，而换行判断是逐字符扫描（成本高）。
    ;; 卡片类组件自带渲染缓存：内容未变时渲染出的是同一批行对象，
    ;; 因此这里缓存上一次的渲染结果与拆分结果，并用“行的对象标识”
    ;; （eq? 指针比较）判定内容是否变化：
    ;;   - 未变化：直接复用拆分结果（零扫描、零分配）
    ;;   - 变化  ：重新逐行扫描拆分（仅在内容真正变化时发生）
    (define (%split-embedded-newlines raw)
      (append-map (lambda (item)
                    (if (string-contains? item "\n") (string-split item "\n") (list item))
                  ) ;lambda
        raw
      ) ;append-map
    ) ;define

    ;; 两份渲染结果的行是否完全相同（按对象标识比较）
    (define (%same-rendered-lines? a b)
      (let loop
        ((x a) (y b))
        (cond ((null? x) (null? y))
              ((null? y) #f)
              ((eq? (car x) (car y)) (loop (cdr x) (cdr y)))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; 取内容行（已拆分内嵌换行）：命中缓存则复用上一份拆分结果
    (define (%scroll-content-lines sv width)
      (let* ((raw (component-render (scroll-view-content sv) width))
             (cached-raw (%scroll-view-rendered sv))
             (cached-split (%scroll-view-split sv))
            ) ;
        (cond ((and cached-raw (eq? cached-raw raw))
               ;; 组件渲染结果未变（同一对象）：拆分结果直接复用
               cached-split
              ) ;
              ((and cached-raw (%same-rendered-lines? raw cached-raw))
               ;; 行对象逐一相同：内容未变，拆分结果仍然有效
               cached-split
              ) ;
              (else
                (let ((split (%split-embedded-newlines raw)))
                  (%scroll-view-rendered-set! sv raw)
                  (%scroll-view-split-set! sv split)
                  ;; 内容变了：行数缓存同时失效，避免上报过期的总行数
                  (%scroll-view-total-set! sv #f)
                  split
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    ;; 显式失效缓存：内容组件内部变化（而非替换组件）时调用，
    ;; 令下一次取内容重新渲染与统计行数。
    (define (scroll-view-invalidate! sv)
      (%check-scroll-view sv "scroll-view-invalidate!")
      (%scroll-view-total-set! sv #f)
      (%scroll-view-rendered-set! sv #f)
      (%scroll-view-split-set! sv #f)
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

    ;; 内容总行数：优先使用上一帧渲染时缓存的行数，
    ;; 缓存失效（内容替换/显式 invalidate）时重新统计。
    (define (scroll-view-total-lines sv)
      (%check-scroll-view sv "scroll-view-total-lines")
      (let ((cached (%scroll-view-total sv)))
        (if cached
          cached
          (let* ((lines (%scroll-content-lines sv (%scroll-view-width sv))) (n (length lines)))
            (%scroll-view-total-set! sv n)
            n
          ) ;let*
        ) ;if
      ) ;let
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
        ;; 缓存内容总行数：滚动时据此换算上界，无需再渲染一遍全部内容
        (%scroll-view-total-set! sv total)
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
