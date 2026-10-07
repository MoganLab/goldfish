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
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii par)
  (import (scheme base) (liii error) (liii go))
  (export par-for-each par-map par-filter vector-par-for-each vector-par-map
    vector-par-filter
  ) ;export
  (begin
    ;; (liii go) worker 结果信封协议：(ok value) 或 (error sym irritants)
    (define (%par-result-error? res)
      (and (pair? res) (eq? (car res) 'error))
    ) ;define

    (define (%par-rethrow! err)
      (apply error (cadr err) (caddr err))
    ) ;define

    (define %par-builtin-predicates
      '(even? odd? zero? positive? negative? number? string? symbol? boolean?
         char? null? pair? vector? integer? real? rational? exact? inexact?)
    ) ;define

    (define (%par-proc-source caller f)
      (let ((src (procedure-source f)))
        (if (pair? src)
          src
          (let ((str (object->string f)))
            (if
              (and (not (string=? str "")) (not (char=? (string-ref str 0) #\#)))
              (let ((sym (string->symbol str)))
                (if (memq sym %par-builtin-predicates)
                  `(lambda (x) (,sym x))
                  (type-error (string-append (symbol->string caller)
                                ": cannot extract source code from procedure"
                              ) ;string-append
                    f
                  ) ;type-error
                ) ;if
              ) ;let
              (type-error (string-append (symbol->string caller)
                            ": cannot extract source code from procedure"
                          ) ;string-append
                f
              ) ;type-error
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%par-make-worker-env f . extra-bindings)
      (let ((e (apply inlet extra-bindings)))
        (let loop
          ((cur (funclet f)))
          (when
            (and (let? cur) (not (eq? cur (rootlet))))
            (varlet e cur)
            (loop (outlet cur))
          ) ;when
        ) ;let
        e
      ) ;let
    ) ;define

    (define (%par-vector-chunks vec)
      (let* ((n (vector-length vec))
             (w (max 1 (go-worker-count)))
             (p (min n w))
             (base (quotient n p))
             (rem (remainder n p))
             (chunks (make-vector p))
            ) ;
        (let loop
          ((k 0) (start 0))
          (if (= k p)
            chunks
            (let* ((size (if (< k rem) (+ base 1) base))
                   (end (+ start size))
                   (sub (vector-copy vec start end))
                  ) ;
              (vector-set! chunks k (vector k start sub))
              (loop (+ k 1) end)
            ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (vector-par-for-each f vec)
      (unless (procedure? f)
        (type-error "vector-par-for-each: first argument must be a procedure" f)
      ) ;unless
      (unless (vector? vec)
        (type-error "vector-par-for-each: second argument must be a vector" vec)
      ) ;unless
      (unless (zero? (vector-length vec))
        (let* ((chunks (%par-vector-chunks vec))
               (p (vector-length chunks))
               (done-ch (make-chan p))
               (src-f (%par-proc-source 'vector-par-for-each f))
               (env (%par-make-worker-env f))
               (worker-proc
                 (eval
                   `(lambda (chunk-meta)
                      (let ((sub (vector-ref chunk-meta 2)))
                        (vector-for-each ,src-f sub)
                        ,#t))
                   env
                 ) ;eval
               ) ;worker-proc
              ) ;

          (let loop
            ((k 0))
            (when (< k p)
              (go-apply worker-proc (list (vector-ref chunks k)) done-ch)
              (loop (+ k 1))
            ) ;when
          ) ;let
          ;; Join Barrier
          (let loop
            ((i 0) (first-error #f))
            (if (= i p)
              (when first-error
                (%par-rethrow! first-error)
              ) ;when
              (let ((res (chan-recv! done-ch)))
                (if (%par-result-error? res)
                  (loop (+ i 1) (or first-error res))
                  (loop (+ i 1) first-error)
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;let*
      ) ;unless
    ) ;define

    (define (vector-par-map f vec)
      (unless (procedure? f)
        (type-error "vector-par-map: first argument must be a procedure" f)
      ) ;unless
      (unless (vector? vec)
        (type-error "vector-par-map: second argument must be a vector" vec)
      ) ;unless
      (if (zero? (vector-length vec))
        #()
        (let* ((n (vector-length vec))
               (chunks (%par-vector-chunks vec))
               (p (vector-length chunks))
               (res-ch (make-chan p))
               (src-f (%par-proc-source 'vector-par-map f))
               (env (%par-make-worker-env f 'res-ch res-ch))
               (worker-proc
                 (eval
                   `(lambda (chunk-meta)
                      (let ((idx (vector-ref chunk-meta 0))
                            (start (vector-ref chunk-meta 1))
                            (sub (vector-ref chunk-meta 2)))
                        (chan-send! res-ch
                          (vector idx start (vector-map ,src-f sub)))
                        ,#t))
                   env
                 ) ;eval
               ) ;worker-proc
              ) ;
          (vector-par-for-each worker-proc chunks)
          (let ((res (make-vector n)))
            (let loop
              ((i 0))
              (if (= i p)
                res
                (let* ((r (chan-recv! res-ch)) (start (vector-ref r 1)) (mapped-sub (vector-ref r 2)))
                  (vector-copy! res start mapped-sub)
                  (loop (+ i 1))
                ) ;let*
              ) ;if
            ) ;let
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

    (define (vector-par-filter pred vec)
      (unless (procedure? pred)
        (type-error "vector-par-filter: first argument must be a procedure" pred)
      ) ;unless
      (unless (vector? vec)
        (type-error "vector-par-filter: second argument must be a vector" vec)
      ) ;unless
      (if (zero? (vector-length vec))
        #()
        (let* ((chunks (%par-vector-chunks vec))
               (p (vector-length chunks))
               (res-ch (make-chan p))
               (src-pred (%par-proc-source 'vector-par-filter pred))
               (env (%par-make-worker-env pred 'res-ch res-ch))
               (worker-proc
                 (eval
                   `(lambda (chunk-meta)
                      (let* ((idx (vector-ref chunk-meta 0))
                             (sub (vector-ref chunk-meta 2))
                             (sub-len (vector-length sub)))
                        (let loop
                          ((j 0) (acc '()))
                          (if (= j sub-len)
                            (begin
                              (chan-send! res-ch
                                (cons idx (list->vector (reverse acc))))
                              #t)
                            (let ((val (vector-ref sub j)))
                              (loop (+ j 1)
                                (if (,src-pred val) (cons val acc) acc)))))))
                   env
                 ) ;eval
               ) ;worker-proc
              ) ;
          (vector-par-for-each worker-proc chunks)
          (let ((filtered-chunks (make-vector p)))
            (let loop
              ((i 0))
              (when (< i p)
                (let ((r (chan-recv! res-ch)))
                  (vector-set! filtered-chunks (car r) (cdr r))
                  (loop (+ i 1))
                ) ;let
              ) ;when
            ) ;let
            (let ((total-len
                    (let sum
                      ((i 0) (acc 0))
                      (if (= i p)
                        acc
                        (sum (+ i 1) (+ acc (vector-length (vector-ref filtered-chunks i))))
                      ) ;if
                    ) ;let
                  ) ;total-len
                 ) ;
              (let ((res (make-vector total-len)))
                (let copy-loop
                  ((i 0) (offset 0))
                  (if (= i p)
                    res
                    (let* ((chunk (vector-ref filtered-chunks i)) (len (vector-length chunk)))
                      (vector-copy! res offset chunk)
                      (copy-loop (+ i 1) (+ offset len))
                    ) ;let*
                  ) ;if
                ) ;let
              ) ;let
            ) ;let
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

    (define (par-for-each f l)
      (unless (procedure? f)
        (type-error "par-for-each: first argument must be a procedure" f)
      ) ;unless
      (unless (list? l)
        (type-error "par-for-each: second argument must be a list" l)
      ) ;unless
      (unless (null? l)
        (let* ((n (length l)) (done-ch (make-chan n)))
          (for-each (lambda (elem) (go-apply f (list elem) done-ch)) l)
          ;; Join Barrier：收满 n 个结果，全部完成后重抛首个异常
          (let loop
            ((i 0) (first-error #f))
            (if (= i n)
              (when first-error
                (%par-rethrow! first-error)
              ) ;when
              (let ((res (chan-recv! done-ch)))
                (if (%par-result-error? res)
                  (loop (+ i 1) (or first-error res))
                  (loop (+ i 1) first-error)
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;let*
      ) ;unless
    ) ;define

    (define (par-map f l)
      (unless (procedure? f)
        (type-error "par-map: first argument must be a procedure" f)
      ) ;unless
      (unless (list? l)
        (type-error "par-map: second argument must be a list" l)
      ) ;unless
      (let ((chans (map (lambda (_) (make-chan 1)) l)))
        (for-each (lambda (elem ch) (go-apply f (list elem) ch)) l chans)
        ;; Join Barrier：按序读取每个专属通道，收满所有结果
        (let loop
          ((chs chans) (results '()) (first-error #f))
          (if (null? chs)
            (if first-error (%par-rethrow! first-error) (reverse results))
            (let ((res (chan-recv! (car chs))))
              (if (%par-result-error? res)
                (loop (cdr chs) results (or first-error res))
                (loop (cdr chs) (cons (cadr res) results) first-error)
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (par-filter pred l)
      (unless (procedure? pred)
        (type-error "par-filter: first argument must be a procedure" pred)
      ) ;unless
      (unless (list? l)
        (type-error "par-filter: second argument must be a list" l)
      ) ;unless
      (if (null? l)
        '()
        (let ((flags (par-map pred l)))
          (let loop
            ((elems l) (fs flags) (acc '()))
            (if (null? elems)
              (reverse acc)
              (loop (cdr elems) (cdr fs) (if (car fs) (cons (car elems) acc) acc))
            ) ;if
          ) ;let
        ) ;let
      ) ;if
    ) ;define
  ) ;begin
) ;define-library
