;;
;; Copyright (C) 2024 The Goldfish Scheme Authors
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

(define-library (liii tui component)
  (export <component> make-component component? component-render
    component-handle-input component-invalidate! component-data
    component-data-set! make-text make-truncated-text make-spacer
  ) ;export
  (import (liii base) (liii error) (liii string) (liii tui text-width))
  (begin

    (define-record-type <component>
      (%make-component-record render handle-input invalidate data)
      component?
      (render %component-render-proc %component-render-proc-set!)
      (handle-input %component-handle-input-proc %component-handle-input-proc-set!)
      (invalidate %component-invalidate-proc %component-invalidate-proc-set!)
      (data %component-data %component-data-set!)
    ) ;define-record-type

    (define <component> '<component>)

    (define (%keyword-value key opts default)
      (let loop
        ((rest opts))
        (cond ((null? rest) default)
              ((and (pair? (cdr rest)) (eq? (car rest) key)) (cadr rest))
              ((null? (cdr rest)) default)
              (else (loop (cddr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (make-component . args)
      (let ((render-fn #f)
            (handle-input-fn (lambda (comp key) #f))
            (invalidate-fn (lambda (comp) #t))
           ) ;
        (cond ((null? args) (type-error "make-component: missing render procedure"))
              ((keyword? (car args))
               (set! render-fn (%keyword-value :render args #f))
               (set! handle-input-fn (%keyword-value :handle-input args handle-input-fn))
               (set! invalidate-fn (%keyword-value :invalidate args invalidate-fn))
              ) ;
              ((procedure? (car args))
               (set! render-fn (car args))
               (when (pair? (cdr args))
                 (set! handle-input-fn (cadr args))
                 (when (pair? (cddr args))
                   (set! invalidate-fn (caddr args))
                 ) ;when
               ) ;when
              ) ;
              (else (type-error "make-component: first argument must be a procedure or keyword")
              ) ;else
        ) ;cond
        (unless (procedure? render-fn)
          (type-error "make-component: render must be a procedure")
        ) ;unless
        (unless (procedure? handle-input-fn)
          (type-error "make-component: handle-input must be a procedure")
        ) ;unless
        (unless (procedure? invalidate-fn)
          (type-error "make-component: invalidate must be a procedure")
        ) ;unless
        (%make-component-record render-fn handle-input-fn invalidate-fn #f)
      ) ;let
    ) ;define

    (define (%call-render-proc proc comp width)
      (let* ((ar (arity proc)) (min-args (car ar)))
        (if (<= min-args 1) (proc width) (proc comp width))
      ) ;let*
    ) ;define

    (define (%call-handle-input-proc proc comp key)
      (let* ((ar (arity proc)) (min-args (car ar)))
        (if (<= min-args 1) (proc key) (proc comp key))
      ) ;let*
    ) ;define

    (define (%call-invalidate-proc proc comp)
      (let* ((ar (arity proc)) (min-args (car ar)))
        (if (= min-args 0) (proc) (proc comp))
      ) ;let*
    ) ;define

    (define (component-render comp width)
      (unless (component? comp)
        (type-error "component-render: comp must be a component")
      ) ;unless
      (unless (real? width)
        (type-error "component-render: width must be a number")
      ) ;unless
      (let ((w (inexact->exact (round width))))
        (%call-render-proc (%component-render-proc comp) comp w)
      ) ;let
    ) ;define

    (define (component-handle-input comp key)
      (unless (component? comp)
        (type-error "component-handle-input: comp must be a component")
      ) ;unless
      (%call-handle-input-proc (%component-handle-input-proc comp) comp key)
    ) ;define

    (define (component-invalidate! comp)
      (unless (component? comp)
        (type-error "component-invalidate!: comp must be a component")
      ) ;unless
      (%call-invalidate-proc (%component-invalidate-proc comp) comp)
    ) ;define

    (define (component-data comp)
      (unless (component? comp)
        (type-error "component-data: comp must be a component")
      ) ;unless
      (%component-data comp)
    ) ;define

    (define (component-data-set! comp val)
      (unless (component? comp)
        (type-error "component-data-set!: comp must be a component")
      ) ;unless
      (%component-data-set! comp val)
    ) ;define

    (define (make-text str)
      (unless (string? str)
        (type-error "make-text: argument must be a string")
      ) ;unless
      (make-component (lambda (comp width) (wrap-text str width)))
    ) ;define

    (define (make-truncated-text str . args)
      (unless (string? str)
        (type-error "make-truncated-text: str must be a string")
      ) ;unless
      (let ((ellipsis (if (null? args) "..." (car args))))
        (unless (string? ellipsis)
          (type-error "make-truncated-text: ellipsis must be a string")
        ) ;unless
        (make-component
          (lambda (comp width)
            (if (<= width 0)
              (list "")
              (let ((first-line (car (string-split str "\n"))))
                (list (truncate-to-width first-line width ellipsis))
              ) ;let
            ) ;if
          ) ;lambda
        ) ;make-component
      ) ;let
    ) ;define

    (define (make-spacer . args)
      (let ((height (if (null? args) 1 (car args))))
        (unless (integer? height)
          (type-error "make-spacer: height must be an integer")
        ) ;unless
        (when (< height 0)
          (value-error "make-spacer: height must be non-negative")
        ) ;when
        (make-component
          (lambda (comp width)
            (let loop
              ((n height) (acc '()))
              (if (<= n 0) acc (loop (- n 1) (cons "" acc)))
            ) ;let
          ) ;lambda
        ) ;make-component
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
