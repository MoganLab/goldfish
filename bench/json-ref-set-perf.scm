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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

;; json-ref / json-set 性能对比基准：C++ 实现 (g_json_ref / g_json_set) vs 历史 Scheme 实现
;; 运行方式: bin/gf bench/json-ref-set-perf.scm

(import (liii base) (liii json) (liii alist) (liii error) (liii timeit))

;; 历史 Scheme 实现（原 (guenchi json) 的 json-ref/json-set 及 (liii json) 的包装），原样保留用于对比

(define g-json-ref/scheme
  (lambda (x k)
    (define return
      (lambda (x)
        (if (symbol? x)
          (cond ((symbol=? x 'true) #t)
                ((symbol=? x 'false) #f)
                (else x)
          ) ;cond
          x
        ) ;if
      ) ;lambda
    ) ;define
    (if (vector? x)
      (return (vector-ref x k))
      (let loop
        ((x x) (k k))
        (if (null? x) '() (if (equal? (caar x) k) (return (cdar x)) (loop (cdr x) k)))
      ) ;let
    ) ;if
  ) ;lambda
) ;define

(define (json-ref/scheme json key . args)
  (if (null? json)
    '()
    (begin
      (unless (or (json-object? json) (json-array? json))
        (type-error "Value is not a JSON object or array" json)
      ) ;unless
      (let ((val (if (and (json-object? json) (equal? json '(())))
                   '()
                   (g-json-ref/scheme json key)
                 ) ;if
            ) ;val
           ) ;
        (if (null? args) val (apply json-ref/scheme (cons val args)))
      ) ;let
    ) ;begin
  ) ;if
) ;define

(define g-json-set/scheme
  (lambda (x v p)
    (let ((x x) (v v) (p (if (procedure? p) p (lambda (x) p))))
      (if (vector? x)
        (list->vector (cond ((boolean? v)
                             (if v
                               (let l
                                 ((x (vector->alist x)) (p p))
                                 (if (null? x) '() (cons (p (cdar x)) (l (cdr x) p)))
                               ) ;let
                             ) ;if
                            ) ;
                            ((procedure? v)
                             (let l
                               ((x (vector->alist x)) (v v) (p p))
                               (if (null? x)
                                 '()
                                 (if (v (caar x))
                                   (cons (p (cdar x)) (l (cdr x) v p))
                                   (cons (cdar x) (l (cdr x) v p))
                                 ) ;if
                               ) ;if
                             ) ;let
                            ) ;
                            (else (let l
                                    ((x (vector->alist x)) (v v) (p p))
                                    (if (null? x)
                                      '()
                                      (if (equal? (caar x) v)
                                        (cons (p (cdar x)) (l (cdr x) v p))
                                        (cons (cdar x) (l (cdr x) v p))
                                      ) ;if
                                    ) ;if
                                  ) ;let
                            ) ;else
                      ) ;cond
        ) ;list->vector
        (cond ((boolean? v)
               (if v
                 (let l
                   ((x x) (p p))
                   (if (null? x) '() (cons (cons (caar x) (p (cdar x))) (l (cdr x) p)))
                 ) ;let
               ) ;if
              ) ;
              ((procedure? v)
               (let l
                 ((x x) (v v) (p p))
                 (if (null? x)
                   '()
                   (if (v (caar x))
                     (cons (cons (caar x) (p (cdar x))) (l (cdr x) v p))
                     (cons (car x) (l (cdr x) v p))
                   ) ;if
                 ) ;if
               ) ;let
              ) ;
              (else (let l
                      ((x x) (v v) (p p))
                      (if (null? x)
                        '()
                        (if (equal? (caar x) v)
                          (cons (cons v (p (cdar x))) (l (cdr x) v p))
                          (cons (car x) (l (cdr x) v p))
                        ) ;if
                      ) ;if
                    ) ;let
              ) ;else
        ) ;cond
      ) ;if
    ) ;let
  ) ;lambda
) ;define

(define (json-set/scheme json key val . args)
  (unless (or (json-object? json) (json-array? json))
    (type-error "Value is not a JSON object or array" json)
  ) ;unless
  (if (null? args)
    (if (and (json-object? json) (equal? json '(())))
      json
      (g-json-set/scheme json key val)
    ) ;if
    (json-set/scheme json
      key
      (lambda (x) (apply json-set/scheme (cons x (cons val args))))
    ) ;json-set/scheme
  ) ;if
) ;define

;; 历史 Scheme 实现（原 (guenchi json) 的 json-push 及 (liii json) 的包装），原样保留用于对比

(define (g-json-push/scheme x k v)
  (if (vector? x)
    (if (= (vector-length x) 0)
      (vector v)
      (list->vector (let l
                      ((x (vector->alist x)) (k k) (v v) (b #f))
                      (if (null? x)
                        (if b '() (cons v '()))
                        (if (equal? (caar x) k)
                          (cons v (cons (cdar x) (l (cdr x) k v #t)))
                          (cons (cdar x) (l (cdr x) k v b))
                        ) ;if
                      ) ;if
                    ) ;let
      ) ;list->vector
    ) ;if
    (cons (cons k v) x)
  ) ;if
) ;define

(define (json-push/scheme json key val . args)
  (unless (or (json-object? json) (json-array? json))
    (type-error "Value is not a JSON object or array" json)
  ) ;unless
  (if (null? args)
    (if (and (json-object? json) (equal? json '(())))
      (g-json-push/scheme '() key val)
      (g-json-push/scheme json key val)
    ) ;if
    (json-set/scheme json
      key
      (lambda (x) (apply json-push/scheme (cons x (cons val args))))
    ) ;json-set/scheme
  ) ;if
) ;define

;; 历史 Scheme 实现（原 (guenchi json) 的 json-drop 及 (liii json) 的包装），原样保留用于对比

(define (g-json-drop/scheme x v)
  (if (vector? x)
    (if (zero? (vector-length x))
      x
      (list->vector (cond ((procedure? v)
                           (let l
                             ((x (vector->alist x)) (v v))
                             (if (null? x) '() (if (v (caar x)) (l (cdr x) v) (cons (cdar x) (l (cdr x) v))))
                           ) ;let
                          ) ;
                          (else (let l
                                  ((x (vector->alist x)) (v v))
                                  (if (null? x)
                                    '()
                                    (if (equal? (caar x) v) (l (cdr x) v) (cons (cdar x) (l (cdr x) v)))
                                  ) ;if
                                ) ;let
                          ) ;else
                    ) ;cond
      ) ;list->vector
    ) ;if
    (cond ((procedure? v)
           (let l
             ((x x) (v v))
             (if (null? x) '() (if (v (caar x)) (l (cdr x) v) (cons (car x) (l (cdr x) v))))
           ) ;let
          ) ;
          (else (let l
                  ((x x) (v v))
                  (if (null? x)
                    '()
                    (if (equal? (caar x) v) (l (cdr x) v) (cons (car x) (l (cdr x) v)))
                  ) ;if
                ) ;let
          ) ;else
    ) ;cond
  ) ;if
) ;define

(define (json-drop/scheme json key . args)
  (unless (or (json-object? json) (json-array? json))
    (type-error "Value is not a JSON object or array" json)
  ) ;unless
  (if (null? args)
    (if (and (json-object? json) (equal? json '(())))
      json
      (g-json-drop/scheme json key)
    ) ;if
    (json-set/scheme json key (lambda (x) (apply json-drop/scheme (cons x args))))
  ) ;if
) ;define

(define (build-json-string n)
  (let ((out (open-output-string)))
    (display "{" out)
    (do ((i 0 (+ i 1)))
      ((= i n))
      (when (> i 0)
        (display "," out)
      ) ;when
      (display "\"k" out)
      (display i out)
      (display "\":" out)
      (display i out)
    ) ;do
    (display "}" out)
    (get-output-string out)
  ) ;let
) ;define

(define bench-obj (string->json (build-json-string 200)))

(define bench-arr (list->vector (iota 200)))

(define bench-nested
  (string->json "{\"user\":{\"id\":1001,\"name\":\"Alice\",\"profile\":{\"age\":21,\"height\":168.5,\"hobbies\":[\"music\",\"reading\"]}},\"scores\":[98,87,93]}"
  ) ;string->json
) ;define

(define bench-ref-key (string-append "k" (number->string 100)))

;; 正确性 sanity check：两种实现输出必须一致
(unless (equal? (json-ref bench-obj bench-ref-key)
          (json-ref/scheme bench-obj bench-ref-key)
        ) ;equal?
  (error 'value-error "object ref mismatch")
) ;unless
(unless (equal? (json-ref bench-nested 'user 'profile 'age)
          (json-ref/scheme bench-nested 'user 'profile 'age)
        ) ;equal?
  (error 'value-error "nested ref mismatch")
) ;unless
(unless (equal? (json-set bench-obj bench-ref-key 0)
          (json-set/scheme bench-obj bench-ref-key 0)
        ) ;equal?
  (error 'value-error "object set mismatch")
) ;unless
(unless (equal? (json-set bench-nested 'user 'profile 'age 22)
          (json-set/scheme bench-nested 'user 'profile 'age 22)
        ) ;equal?
  (error 'value-error "nested set mismatch")
) ;unless
(unless (equal? (json-set bench-arr #t (lambda (x) (* x 2)))
          (json-set/scheme bench-arr #t (lambda (x) (* x 2)))
        ) ;equal?
  (error 'value-error "array map-set mismatch")
) ;unless
(unless (equal? (json-push bench-obj "newkey" 'new)
          (json-push/scheme bench-obj "newkey" 'new)
        ) ;equal?
  (error 'value-error "object push mismatch")
) ;unless
(unless (equal? (json-push bench-arr 300 'new) (json-push/scheme bench-arr 300 'new))
  (error 'value-error "array push mismatch")
) ;unless
(unless (equal? (json-push bench-nested 'user 'profile 'weight 60)
          (json-push/scheme bench-nested 'user 'profile 'weight 60)
        ) ;equal?
  (error 'value-error "nested push mismatch")
) ;unless
(unless (equal? (json-drop bench-obj bench-ref-key)
          (json-drop/scheme bench-obj bench-ref-key)
        ) ;equal?
  (error 'value-error "object drop mismatch")
) ;unless
(unless (equal? (json-drop bench-arr 100) (json-drop/scheme bench-arr 100))
  (error 'value-error "array drop mismatch")
) ;unless
(unless (equal? (json-drop bench-nested 'user 'profile 'age)
          (json-drop/scheme bench-nested 'user 'profile 'age)
        ) ;equal?
  (error 'value-error "nested drop mismatch")
) ;unless

(define ref-iter 2000)

(define set-iter 500)

(define (report title iterations time-val)
  (display "[")
  (display title)
  (display "] iterations=")
  (display iterations)
  (display " time=")
  (display time-val)
  (display "s")
  (newline)
) ;define

;; warmup
(do ((i 0 (+ i 1)))
  ((= i 20))
  (json-ref bench-obj bench-ref-key)
  (json-ref/scheme bench-obj bench-ref-key)
  (json-set bench-obj bench-ref-key 0)
  (json-set/scheme bench-obj bench-ref-key 0)
  (json-push bench-obj "newkey" 'new)
  (json-push/scheme bench-obj "newkey" 'new)
  (json-drop bench-obj bench-ref-key)
  (json-drop/scheme bench-obj bench-ref-key)
) ;do

(display "=== json-ref / json-set C++ vs Scheme 性能对比 ===")
(newline)
(newline)

(let ((t (timeit (lambda () (json-ref bench-obj bench-ref-key)) '() ref-iter)))
  (report "对象单键读取/C++" ref-iter t)
) ;let
(let ((t (timeit (lambda () (json-ref/scheme bench-obj bench-ref-key)) '() ref-iter))
     ) ;
  (report "对象单键读取/Scheme" ref-iter t)
) ;let

(let ((t (timeit (lambda () (json-ref bench-nested 'user 'profile 'age)) '() ref-iter)
      ) ;t
     ) ;
  (report "多键路径读取/C++" ref-iter t)
) ;let
(let ((t (timeit (lambda () (json-ref/scheme bench-nested 'user 'profile 'age))
           '()
           ref-iter
         ) ;timeit
      ) ;t
     ) ;
  (report "多键路径读取/Scheme" ref-iter t)
) ;let

(let ((t (timeit (lambda () (json-set bench-obj bench-ref-key 0)) '() set-iter)))
  (report "对象单键设置/C++" set-iter t)
) ;let
(let ((t (timeit (lambda () (json-set/scheme bench-obj bench-ref-key 0)) '() set-iter)
      ) ;t
     ) ;
  (report "对象单键设置/Scheme" set-iter t)
) ;let

(let ((t (timeit (lambda () (json-set bench-nested 'user 'profile 'age 22)) '() set-iter)
      ) ;t
     ) ;
  (report "多键路径设置/C++" set-iter t)
) ;let
(let ((t (timeit (lambda () (json-set/scheme bench-nested 'user 'profile 'age 22))
           '()
           set-iter
         ) ;timeit
      ) ;t
     ) ;
  (report "多键路径设置/Scheme" set-iter t)
) ;let

(let ((t (timeit (lambda () (json-set bench-arr #t (lambda (x) (* x 2)))) '() set-iter)
      ) ;t
     ) ;
  (report "数组全映射设置/C++" set-iter t)
) ;let
(let ((t (timeit (lambda () (json-set/scheme bench-arr #t (lambda (x) (* x 2))))
           '()
           set-iter
         ) ;timeit
      ) ;t
     ) ;
  (report "数组全映射设置/Scheme" set-iter t)
) ;let

(let ((t (timeit (lambda () (json-push bench-obj "newkey" 'new)) '() set-iter)))
  (report "对象单键前插/C++" set-iter t)
) ;let
(let ((t (timeit (lambda () (json-push/scheme bench-obj "newkey" 'new)) '() set-iter))
     ) ;
  (report "对象单键前插/Scheme" set-iter t)
) ;let

(let ((t (timeit (lambda () (json-push bench-arr 300 'new)) '() set-iter)))
  (report "数组无匹配尾插/C++" set-iter t)
) ;let
(let ((t (timeit (lambda () (json-push/scheme bench-arr 300 'new)) '() set-iter)))
  (report "数组无匹配尾插/Scheme" set-iter t)
) ;let

(let ((t (timeit (lambda () (json-push bench-nested 'user 'profile 'weight 60))
           '()
           set-iter
         ) ;timeit
      ) ;t
     ) ;
  (report "多键路径前插/C++" set-iter t)
) ;let
(let ((t (timeit (lambda () (json-push/scheme bench-nested 'user 'profile 'weight 60))
           '()
           set-iter
         ) ;timeit
      ) ;t
     ) ;
  (report "多键路径前插/Scheme" set-iter t)
) ;let

(let ((t (timeit (lambda () (json-drop bench-obj bench-ref-key)) '() set-iter)))
  (report "对象单键删除/C++" set-iter t)
) ;let
(let ((t (timeit (lambda () (json-drop/scheme bench-obj bench-ref-key)) '() set-iter))
     ) ;
  (report "对象单键删除/Scheme" set-iter t)
) ;let

(let ((t (timeit (lambda () (json-drop bench-arr 100)) '() set-iter)))
  (report "数组索引删除/C++" set-iter t)
) ;let
(let ((t (timeit (lambda () (json-drop/scheme bench-arr 100)) '() set-iter)))
  (report "数组索引删除/Scheme" set-iter t)
) ;let

(let ((t (timeit (lambda () (json-drop bench-nested 'user 'profile 'age)) '() set-iter)
      ) ;t
     ) ;
  (report "多键路径删除/C++" set-iter t)
) ;let
(let ((t (timeit (lambda () (json-drop/scheme bench-nested 'user 'profile 'age))
           '()
           set-iter
         ) ;timeit
      ) ;t
     ) ;
  (report "多键路径删除/Scheme" set-iter t)
) ;let

(newline)
(display "=== 测试完成 ===")
(newline)
