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

(define-library (liii tui box container)
  (export make-container container? container-add! container-clear!
    container-children
  ) ;export
  (import (scheme base)
    (liii base)
    (liii error)
    (liii tui component)
    (liii tui mouse)
    (liii tui box vstack)
  ) ;import
  (begin

    (define-record-type <container-data>
      (%make-container-data children mouse-layout)
      %container-data?
      (children %container-data-children %container-data-children-set!)
      ;; 鼠标布局缓存：(width . ((child . height) ...))，渲染时登记
      (mouse-layout %container-data-mouse-layout %container-data-mouse-layout-set!)
    ) ;define-record-type

    (define (container? obj)
      (and (component? obj) (%container-data? (component-data obj)))
    ) ;define

    (define (%check-container comp caller)
      (unless (container? comp)
        (type-error (string-append caller ": expected a container"))
      ) ;unless
    ) ;define

    ;; children 在 data 中倒序存储，container-add! 为 O(1)；读取时 reverse
    (define (container-children comp)
      (%check-container comp "container-children")
      (reverse (%container-data-children (component-data comp)))
    ) ;define

    (define (container-add! comp child)
      (%check-container comp "container-add!")
      (let ((c (%validate-child child "container-add!")) (data (component-data comp)))
        (%container-data-children-set! data (cons c (%container-data-children data)))
        (%container-data-mouse-layout-set! data #f)
        comp
      ) ;let
    ) ;define

    (define (container-clear! comp)
      (%check-container comp "container-clear!")
      (%container-data-children-set! (component-data comp) '())
      (%container-data-mouse-layout-set! (component-data comp) #f)
      comp
    ) ;define

    ;; container 即 children 可变、spacing 为 0 的 vstack，渲染/派发/失效直接复用
    (define (make-container)
      (let ((comp
              (make-component
                :render
                (lambda (self width)
                  (let* ((children (container-children self))
                         (lines (%vstack-render children 0 width))
                         (heights
                           (map (lambda (child) (length (component-render child width))) children)
                         ) ;heights
                        ) ;
                    (%container-data-mouse-layout-set! (component-data self)
                      (cons width (map cons children heights))
                    ) ;%container-data-mouse-layout-set!
                    lines
                  ) ;let*
                ) ;lambda
                :handle-input
                (lambda (self key) (%vstack-handle-input (container-children self) key))
                :handle-mouse
                (lambda (self ev) (%container-handle-mouse self ev))
                :invalidate
                (lambda (self) (%vstack-invalidate (container-children self)))
              ) ;make-component
            ) ;comp
           ) ;
        (component-data-set! comp (%make-container-data '() #f))
        comp
      ) ;let
    ) ;define

    ;; 鼠标按布局命中：y 落入哪个子组件的行区间就派发给谁（局部坐标），
    ;; 未命中或子组件未处理时返回 #f；布局缓存与当前渲染宽度不一致时
    ;; 重新渲染推导，对齐上游 pi Container.handleMouse 的 mouseLayout 机制
    (define (%container-handle-mouse self ev)
      (let* ((children (container-children self))
             (x (mouse-event-x ev))
             (y (mouse-event-y ev))
             (heights
               (map
                 (lambda (child)
                   (let ((layout (%container-data-mouse-layout (component-data self))))
                     (if (and (pair? layout) (assoc child (cdr layout)))
                       (cdr (assoc child (cdr layout)))
                       (length (component-render child 40))
                     ) ;if
                   ) ;let
                 ) ;lambda
                 children
               ) ;map
             ) ;heights
            ) ;
        (%hit-dispatch children heights x y ev)
      ) ;let*
    ) ;define

    ;; 按高度区间顺序命中：命中后把 y 换算为子组件局部坐标派发
    (define (%hit-dispatch children heights x y ev)
      (let loop
        ((rest children) (offsets heights) (base 0))
        (if (null? rest)
          #f
          (let ((h (car offsets)))
            (cond
             ((and (>= y base) (< y (+ base h)))
              (component-handle-mouse! (car rest)
                (make-mouse-event (mouse-event-type ev)
                  (mouse-event-button ev)
                  x
                  (- y base)
                  (mouse-event-shift? ev)
                  (mouse-event-ctrl? ev)
                  (mouse-event-alt? ev)
                ) ;make-mouse-event
              ) ;component-handle-mouse!
             ) ;
             (else (loop (cdr rest) (cdr offsets) (+ base h)))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
