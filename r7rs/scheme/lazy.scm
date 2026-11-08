;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; (scheme lazy) - R7RS-small 4.2.5 delayed evaluation
;;
;; 实现基于 SRFI 45 (André van Tonder, 2004) 的参考实现思路：
;; - promise 分三态：eager（已求值）、lazy（delay 产物，thunk 求值得到值）、
;;   recursive（delay-force 产物，thunk 求值得到下一个 promise 或值）。
;; - force 对 lazy 态采用"求值后重取"（SRFI 45 参考实现中标注 ; * 的两行）：
;;   重入的 force 会再次执行 thunk，若求值期间 promise 已被内层 force 完成，
;;   则采用内层记忆化的结果，覆盖外层的过期值。
;; - force 对 recursive 态采用蹦床（trampoline）跳转：thunk 返回 promise 时
;;   直接在其上继续循环，不在栈上累积，保证迭代惰性算法在有界空间内运行
;;   （R7RS 4.2.5：forcing delay-force 结果对内层 force 构成尾调用）。
;;   跳转不回写中间 promise：链条自然流入最终被记忆化的 promise，
;;   与 SRFI 45 共享 pair 嫁接的可观察行为一致（memoization test 3）。
;; - s7 的 let 不能绑定 multiple-values，故 thunk 结果经 call-with-values
;;   取回：单值走记忆化；多值直接透传（R7RS 对多值 delay 行为未指定，
;;   此处选择支持而不报错）。

(define-library (scheme lazy)
  (import (scheme base))
  (export delay delay-force force make-promise promise?)
  (begin

    (define-record-type <promise>
      (%make-promise tag thunk-or-value)
      promise?
      (tag %promise-tag %promise-tag-set!)
      (thunk-or-value %promise-value %promise-value-set!)
    ) ;define-record-type

    ;; (delay expression) 构造延迟求值 expression 的 promise
    (define-syntax delay
      (syntax-rules ()
        ((delay expression) (%make-promise 'lazy (lambda () expression)))
      ) ;syntax-rules
    ) ;define-syntax

    ;; (delay-force expression) 语义上等价于 (delay (force expression))，
    ;; 区别在于 thunk 不预求值：返回下一个 promise 交给 force 蹦床跳转，
    ;; 从而兑现 R7RS 的尾调用（有界空间）保证
    (define-syntax delay-force
      (syntax-rules ()
        ((delay-force expression) (%make-promise 'recursive (lambda () expression)))
      ) ;syntax-rules
    ) ;define-syntax

    (define (make-promise obj)
      (if (promise? obj) obj (%make-promise 'eager obj))
    ) ;define

    (define (force promise)
      (if (promise? promise)
        (let loop
          ((p promise))
          (if (eq? (%promise-tag p) 'eager)
            (%promise-value p)
            (call-with-values (lambda () ((%promise-value p)))
              (lambda x
                (cond ((eq? (%promise-tag p) 'eager)
                       ;; 求值期间被重入 force 完成：丢弃本层 thunk 返回值，
                       ;; 采用内层已记忆化的结果（SRFI 45 重入语义）
                       (%promise-value p)
                      ) ;
                      ((and (eq? (%promise-tag p) 'recursive) (= (length x) 1) (promise? (car x)))
                       ;; 蹦床跳转到下一个 promise，不增长栈
                       (loop (car x))
                      ) ;
                      ((= (length x) 1)
                       ;; delay 的值，或 delay-force 求值结果为值：记忆化
                       (%promise-tag-set! p 'eager)
                       (%promise-value-set! p (car x))
                       (car x)
                      ) ;
                      (else
                        ;; 多值：不记忆化，直接透传（R7RS 未指定多值行为）
                        (apply values x)
                      ) ;else
                ) ;cond
              ) ;lambda
            ) ;call-with-values
          ) ;if
        ) ;let
        ;; R7RS：非 promise 参数允许原样返回
        promise
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
