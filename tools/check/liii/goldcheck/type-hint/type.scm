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

;; 表达式类型推导：hint 签名 + 局部绑定 -> 表达式分析类型。
;; 无法确定的表达式返回 #f（unknown），unknown 不参与冲突判定。

(define-library (liii goldcheck type-hint type)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii goldcheck type-hint parse)
    (liii goldcheck type-hint hint-data)
    (liii goldcheck type-hint compat)
    (liii goldcheck type-hint value)
  ) ;import
  (export infer-type call-return-type call-args-ok? inline-assq-access?)
  (begin

    ;; 候选签名的公共返回类型：唯一则返回该类型，否则 #f
    ;; 注意：(boolean? #f) 为 #t（R7RS 语义），故 #f 字面量必须先行判定，
    ;; 否则 #f 会被误推导为 boolean? 而产生假阳性。
    (define (call-return-type sigs expr env locals)
      (let ((survivors (filter (lambda (s) (call-args-ok? s expr env locals)) sigs)))
        (if (null? survivors)
          #f
          (same-return-type
            (map
              (lambda (s) (let ((p (signature-parts s))) (if p (cadr p) #f)))
              survivors
            ) ;map
          ) ;same-return-type
        ) ;if
      ) ;let
    ) ;define

    (define (same-return-type rets)
      (let loop
        ((rs rets))
        (cond ((null? rs) (car rets))
              ((not (equal? (car rs) (car rets))) #f)
              (else (loop (cdr rs)))
        ) ;cond
      ) ;let
    ) ;define

    ;; (cdr (assq k a)) 等内联取值惯用法：对静态构造的 alist，
    ;; 键必然存在，将其视为存在性已保证，不作为 #f 来源传播，
    ;; 避免对存量惯用风格大规模误报（spec 8.9 优先避免误报）。
    (define %accessors '(car cdr caar cadr cdar cddr caddr cdddr cadddr))

    (define (inline-assq-access? expr)
      (and (pair? expr)
        (memq (car expr) %accessors)
        (pair? (cdr expr))
        (pair? (cadr expr))
        (memq (car (cadr expr)) '(assq assoc))
        #t
      ) ;and
    ) ;define

    (define (infer-type expr env locals)
      (cond ((eq? expr #f) #f)
            ((inline-assq-access? expr) #f)
            ((integer? expr) 'integer?)
            ((number? expr) 'number?)
            ((string? expr) 'string?)
            ((boolean? expr) 'boolean?)
            ((char? expr) 'char?)
            ((symbol? expr) (let ((local (assq expr locals))) (if local (cdr local) #f)))
            ((and (pair? expr) (quote-marker? (car expr)) (pair? (cdr expr)))
             (let ((x (cadr expr)))
               (cond ((symbol? x) 'symbol?)
                     ((null? x) 'null?)
                     ((integer? x) 'integer?)
                     (else #f)
               ) ;cond
             ) ;let
            ) ;
            ((and (pair? expr) (eq? (car expr) 'cond))
             (infer-cond infer-type expr env locals)
            ) ;
            ((and (pair? expr) (symbol? (car expr)))
             (let ((local (assq expr locals)))
               (if local
                 (cdr local)
                 (let ((sigs (assq (car expr) env)))
                   (if (not sigs) #f (call-return-type (cdr sigs) expr env locals))
                 ) ;let
               ) ;if
             ) ;let
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    ;; 签名是否与实参列表兼容（用于筛选候选签名）
    (define (call-args-ok? sig expr env locals)
      (let ((p (signature-parts sig)))
        (and p
          (proper-list? (cdr expr))
          (= (length (car p)) (length (cdr expr)))
          (let loop
            ((params (car p)) (args (cdr expr)))
            (cond ((null? params) #t)
                  ((%accepts? (car params) (infer-type (car args) env locals))
                   (loop (cdr params) (cdr args))
                  ) ;
                  (else #f)
            ) ;cond
          ) ;let
        ) ;and
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
