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
                         (res (%vstack-render-layout children 0 width))
                         (lines (car res))
                        ) ;
                    (%container-data-mouse-layout-set! (component-data self) (cons width (cdr res)))
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

    ;; 鼠标按布局命中：未渲染过（无布局缓存）时不命中；
    ;; 命中后 y 换算为子组件局部坐标派发（对齐上游 pi Container.handleMouse）
    (define (%container-handle-mouse self ev)
      (let* ((layout (%container-data-mouse-layout (component-data self))))
        (if layout
          (%vstack-dispatch-mouse (container-children self) (cdr layout) 0 ev)
          #f
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
