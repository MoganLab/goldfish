;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; mouse-region 包装器（对齐上游 pi 的 MouseRegion 组件）：
;; 给任意组件附加鼠标处理能力而不改变其渲染与键盘输入。
;; 收到鼠标事件时先派发给被包装的子组件，未被处理时再调用自身回调。

(define-library (liii tui box mouse-region)
  (export make-mouse-region mouse-region? mouse-region-child)
  (import (scheme base) (liii base) (liii error) (liii tui component))
  (begin

    (define-record-type <mouse-region-data>
      (%make-mouse-region-data child on-mouse)
      %mouse-region-data?
      (child %mouse-region-child)
      (on-mouse %mouse-region-on-mouse)
    ) ;define-record-type

    (define (mouse-region? obj)
      (and (component? obj) (%mouse-region-data? (component-data obj)))
    ) ;define

    (define (mouse-region-child comp)
      (unless (mouse-region? comp)
        (type-error "mouse-region-child: expected a mouse-region")
      ) ;unless
      (%mouse-region-child (component-data comp))
    ) ;define

    (define (make-mouse-region child on-mouse)
      (unless (component? child)
        (type-error "make-mouse-region: child must be a component")
      ) ;unless
      (unless (procedure? on-mouse)
        (type-error "make-mouse-region: on-mouse must be a procedure")
      ) ;unless
      (let ((comp
              (make-component
                :render (lambda (self width) (component-render child width))
                :handle-input
                (lambda (self key) (component-handle-input child key))
                :handle-mouse
                (lambda (self ev)
                  (let ((inner (component-handle-mouse! child ev)))
                    (if inner inner ((%mouse-region-on-mouse (component-data self)) ev))
                  ) ;let
                ) ;lambda
                :invalidate (lambda (self) (component-invalidate! child))
              ) ;make-component
            ) ;comp
           ) ;
        (component-data-set! comp (%make-mouse-region-data child on-mouse))
        comp
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
