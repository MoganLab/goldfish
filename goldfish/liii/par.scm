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
  (import (scheme base) (liii base) (liii error) (liii go))
  (export par-for-each)
  (begin
    (define (%get-active-libs)
      (catch #t
        (lambda ()
          (if (and (defined? '*r7rs-libraries*) (hash-table? *r7rs-libraries*))
            (map car *r7rs-libraries*)
            '()
          ) ;if
        ) ;lambda
        (lambda (t a) '())
      ) ;catch
    ) ;define

    (define (%param-names args)
      (cond ((null? args) '())
            ((symbol? args) (list args))
            ((pair? args)
             (let ((first (car args)))
               (cons (if (pair? first) (car first) first) (%param-names (cdr args)))
             ) ;let
            ) ;
            (else '())
      ) ;cond
    ) ;define

    (define (%serializable-data? val)
      (and (not (undefined? val))
        (not (procedure? val))
        (not (syntax? val))
        (not (macro? val))
      ) ;and
    ) ;define

    (define (%free-vars fn)
      (let ((src (procedure-source fn)))
        (if (not (pair? src))
          '()
          (let* ((params (if (pair? (cdr src)) (%param-names (cadr src)) '()))
                 (env (funclet fn))
                 (bindings '())
                ) ;
            (let walk
              ((x (cddr src)))
              (cond
               ((and (pair? x) (eq? (car x) 'quote)) #f)
               ((symbol? x)
                (if (and (not (memq x params)) (not (assq x bindings)) (defined? x env))
                  (let ((val (catch #t (lambda () (let-ref env x)) (lambda (t a) (if #f #f)))))
                    (if (%serializable-data? val) (set! bindings (cons (cons x val) bindings)))
                  ) ;let
                ) ;if
               ) ;
               ((pair? x) (walk (car x)) (walk (cdr x)))
               ((vector? x) (for-each walk (vector->list x)))
              ) ;cond
            ) ;let
            bindings
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (par-for-each f l)
      (if (not (procedure? f))
        (error 'type-error "par-for-each: first argument must be a procedure" f)
      ) ;if
      (if (not (list? l))
        (error 'type-error "par-for-each: second argument must be a list" l)
      ) ;if
      (if (null? l)
        (if #f #f)
        (let ((src (procedure-source f)))
          (if (not (pair? src))
            (error 'type-error "par-for-each: cannot extract source code from procedure" f)
          ) ;if
          (let* ((n (length l))
                 (done-ch (make-chan n))
                 (done-ch-sym (gensym "done-ch"))
                 (elem-sym (gensym "elem"))
                 (captured (catch #t (lambda () (%free-vars f)) (lambda (t a) '())))
                 (captured-names (map car captured))
                 (captured-vals (map cdr captured))
                 (names (cons done-ch-sym (cons elem-sym captured-names)))
                 (libs (%get-active-libs))
                 (code
                   `(begin
                      (set! *load-path* (quote ,*load-path*))
                      ,@(if (null? libs) '() `((import ,@libs)))
                      (catch ,#t
                        (lambda ,()
                          (,src ,elem-sym)
                          (chan-send! ,done-ch-sym '(ok)))
                        (lambda (tag args)
                          (catch ,#t
                            (lambda ,()
                              (chan-send! ,done-ch-sym (list 'error tag args)))
                            (lambda (t2 a2)
                              (chan-send! ,done-ch-sym
                                (list 'error tag (list (object->string args)))))))))
                 ) ;code
                ) ;
            (for-each
              (lambda (elem) (g_go-spawn names (cons done-ch (cons elem captured-vals)) code))
              l
            ) ;for-each
            (let loop
              ((i 0) (first-error #f))
              (if (= i n)
                (if first-error (apply error (cadr first-error) (caddr first-error)) (if #f #f))
                (let ((res (chan-recv! done-ch)))
                  (if (and (pair? res) (eq? (car res) 'error))
                    (loop (+ i 1) (if first-error first-error res))
                    (loop (+ i 1) first-error)
                  ) ;if
                ) ;let
              ) ;if
            ) ;let
          ) ;let*
        ) ;let
      ) ;if
    ) ;define
  ) ;begin
) ;define-library
