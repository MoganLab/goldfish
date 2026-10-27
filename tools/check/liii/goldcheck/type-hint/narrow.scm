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

;; 类型收窄：利用 and / if / cond 的守卫条件，把 (or #f T) 收窄为 T。
;; 收窄只影响局部变量的类型推导，不改变检查的严格程度。

(define-library (liii goldcheck type-hint narrow)
  (import (scheme base) (liii base) (liii list))
  (export narrow-type narrow-locals test-assumptions apply-assumptions walk-if
    walk-and walk-cond
  ) ;export
  (begin

    ;; 可用于收窄的类型判定谓词
    (define %type-predicates
      '(string? pair? list? null? integer? number? symbol? char? vector?
         procedure? boolean?)
    ) ;define

    ;; 按假设收窄类型：t 为 (or #f T ...) 时，去掉 #f 后即为收窄结果
    ;; pred 为 'truthy（假设非假）时收窄为 T；pred 为具体类型符号时，
    ;; 该类型在候选中才收窄，否则保持原类型。
    (define (narrow-type t pred)
      (cond ((not t) t)
            ((and (pair? t) (eq? (car t) 'or))
             (let ((members (filter (lambda (m) (not (eq? m #f))) (cdr t))))
               (cond ((null? members) t)
                     ((eq? pred 'truthy) (if (null? (cdr members)) (car members) (cons 'or members)))
                     ((memq pred members) pred)
                     (else t)
               ) ;cond
             ) ;let
            ) ;
            (else t)
      ) ;cond
    ) ;define

    ;; 局部类型表中某个变量的类型按假设收窄
    (define (narrow-locals locals name pred)
      (if (not (and name (symbol? name)))
        locals
        (let loop
          ((ls locals))
          (cond ((null? ls) locals)
                ((eq? (caar ls) name)
                 (let ((nt (narrow-type (cdar ls) pred)))
                   (if (eq? nt (cdar ls)) ls (cons (cons name nt) (cdr ls)))
                 ) ;let
                ) ;
                (else (loop (cdr ls)))
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    ;; 测试表达式 -> 收窄假设列表 ((变量 . 假设) ...)
    ;; 假设为 'truthy（非假）或具体类型符号（如 string?）。
    ;; and 的各合取项都为真；or 无法确定哪项为真；not 不做收窄。
    (define (test-assumptions test)
      (cond
       ((and (pair? test) (eq? (car test) 'and))
        (apply append (map test-assumptions (cdr test)))
       ) ;
       ((and (pair? test) (eq? (car test) 'or)) '())
       ((and (pair? test) (eq? (car test) 'not)) '())
       ((symbol? test) (list (cons test 'truthy)))
       ((and (pair? test) (symbol? (car test)) (memq (car test) %type-predicates))
        (if (and (pair? (cdr test)) (symbol? (cadr test)))
          (list (cons (cadr test) (car test)))
          '()
        ) ;if
       ) ;
       (else '())
      ) ;cond
    ) ;define

    ;; 依次应用收窄假设
    (define (apply-assumptions locals assumps)
      (if (null? assumps)
        locals
        (apply-assumptions (narrow-locals locals (caar assumps) (cdar assumps))
          (cdr assumps)
        ) ;apply-assumptions
      ) ;if
    ) ;define

    ;; if 形式：test 为真时收窄 then 分支
    (define (walk-if walk rel expr env locals acc)
      (let* ((test (cadr expr))
             (then-expr (caddr expr))
             (else-expr (if (> (length expr) 3) (cadddr expr) #f))
             (acc1 (walk rel test env locals acc))
             (locals-t (apply-assumptions locals (test-assumptions test)))
            ) ;
        (let ((acc2 (walk rel then-expr env locals-t acc1)))
          (if else-expr (walk rel else-expr env locals acc2) acc2)
        ) ;let
      ) ;let*
    ) ;define

    ;; and 形式：后续操作数按前面的操作数已为真收窄
    (define (walk-and walk rel expr env locals acc)
      (let loop
        ((ops (cdr expr)) (locals locals) (a acc))
        (if (null? ops)
          a
          (let* ((op (car ops))
                 (a1 (walk rel op env locals a))
                 (locals1 (apply-assumptions locals (test-assumptions op)))
                ) ;
            (loop (cdr ops) locals1 a1)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; cond 形式：各分支按其测试为真收窄
    (define (walk-cond walk walk-body rel expr env locals acc)
      (let loop
        ((clauses (cdr expr)) (a acc))
        (if (null? clauses)
          a
          (let* ((clause (car clauses))
                 (test (car clause))
                 (body (cdr clause))
                 (a1 (walk rel test env locals a))
                 (locals1 (apply-assumptions locals (test-assumptions test)))
                ) ;
            (loop (cdr clauses) (walk-body rel body env locals1 a1))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
