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

;; 值形式（首版：cond）的类型聚合：cond 作为表达式时，其类型为各子句
;; 末表达式的聚合。infer-type 经参数注入以避免与 infer 模块循环依赖。

(define-library (liii goldcheck type-hint value)
  (import (scheme base) (liii base) (liii list))
  (export infer-cond)
  (begin

    ;; 类型 -> 成员列表： 展开，单类型包成列表
    (define (%type-members t)
      (if (and (pair? t) (eq? (car t) 'or)) (cdr t) (list t))
    ) ;define

    ;; 成员列表 -> 类型：去重；单元素直接返回；空列表 #f
    (define (%make-type ms)
      (let loop
        ((l ms) (acc '()))
        (cond
         ((null? l)
          (let ((dedup (reverse acc)))
            (cond ((null? dedup) #f)
                  ((null? (cdr dedup)) (car dedup))
                  (else (cons 'or dedup))
            ) ;cond
          ) ;let
         ) ;
         ((member (car l) acc equal?) (loop (cdr l) acc))
         (else (loop (cdr l) (cons (car l) acc)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 类型列表聚合：任一 unknown（#f）则整体 unknown
    (define (%union-types ts)
      (if (any not ts) #f (%make-type (apply append (map %type-members ts))))
    ) ;define

    ;; 聚合视角的类型：字面量 #f 为假值 boolean?，其余走 infer-type
    (define (%value-type infer-type e env locals)
      (if (eq? e #f) 'boolean? (infer-type e env locals))
    ) ;define

    ;; 子句值：body 末表达式；=> 子句与空 body 保守 unknown
    (define (clause-value infer-type clause env locals)
      (let ((test (car clause)) (body (cdr clause)))
        (cond ((null? body) #f)
              ((and (pair? test) (pair? (cdr test)) (eq? (cadr test) '=>')) #f)
              (else (infer-type (car (reverse body)) env locals))
        ) ;cond
      ) ;let
    ) ;define

    ;; cond 值：各子 clause 末表达式聚合；无 else 时并入假值
    (define (infer-cond infer-type expr env locals)
      (let loop
        ((cs (cdr expr)) (ts '()) (has-else? #f))
        (cond
         ((null? cs) (%union-types (if has-else? ts (cons 'boolean? ts))))
         ((eq? (caar cs) 'else)
          (loop (cdr cs) (cons (clause-value infer-type (car cs) env locals) ts) #t)
         ) ;
         (else
           (loop (cdr cs)
             (cons (clause-value infer-type (car cs) env locals) ts)
             has-else?
           ) ;loop
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
