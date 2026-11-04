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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii error)
  (import (scheme base))
  (export ??? os-error file-not-found-error not-a-directory-error
    file-exists-error timeout-error type-error type-error? key-error value-error
    index-error permission-denied-error process-not-found-error file-error
    catch-error->string extract-unbound-variable extract-error-location
    format-error-location unbound-variable-hint
  ) ;export
  (begin

    (define (os-error . args)
      (apply error (cons 'os-error args))
    ) ;define

    (define (permission-denied-error . args)
      (apply error (cons 'permission-denied-error args))
    ) ;define

    (define (process-not-found-error . args)
      (apply error (cons 'process-not-found-error args))
    ) ;define

    (define (file-not-found-error . args)
      (apply error (cons 'file-not-found-error args))
    ) ;define

    (define (file-error . args)
      (apply error (cons 'file-error args))
    ) ;define

    (define (not-a-directory-error . args)
      (apply error (cons 'not-a-directory-error args))
    ) ;define

    (define (file-exists-error . args)
      (apply error (cons 'file-exists-error args))
    ) ;define

    (define (timeout-error . args)
      (apply error (cons 'timeout-error args))
    ) ;define

    (define (type-error . args)
      (apply error (cons 'type-error args))
    ) ;define

    (define (type-error? err)
      (if (member err '(type-error wrong-type-arg)) #t #f)
    ) ;define

    (define (key-error . args)
      (apply error (cons 'key-error args))
    ) ;define

    (define (value-error . args)
      (apply error (cons 'value-error args))
    ) ;define

    (define (index-error . args)
      (apply error (cons 'index-error args))
    ) ;define

    (define (??? . args)
      (apply error (cons '??? args))
    ) ;define

    (define (%normalize-fmt-args info)
      (cond ((string? info) (list info))
            ((and (pair? info) (pair? (car info)) (pair? (caar info)) (string? (caaar info)))
             (caar info)
            ) ;
            ((and (pair? info) (pair? (car info)) (string? (caar info))) (car info))
            ((and (pair? info) (string? (car info))) info)
            (else #f)
      ) ;cond
    ) ;define

    (define (%find-pair-location expr)
      (cond ((not (pair? expr)) #f)
            ((and (pair-filename expr) (pair-line-number expr))
             (cons (pair-filename expr) (pair-line-number expr))
            ) ;
            (else
              (let loop
                ((p expr))
                (cond ((not (pair? p)) #f)
                      ((and (pair? (car p)) (pair-filename (car p)) (pair-line-number (car p)))
                       (cons (pair-filename (car p)) (pair-line-number (car p)))
                      ) ;
                      ((pair? (car p)) (or (%find-pair-location (car p)) (loop (cdr p))))
                      (else (loop (cdr p)))
                ) ;cond
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (extract-error-location info)
      (let* ((fmt-args (%normalize-fmt-args info))
             (expr (and (pair? fmt-args)
                     (pair? (cdr fmt-args))
                     (pair? (cddr fmt-args))
                     (caddr fmt-args)
                   ) ;and
             ) ;expr
             (from-expr (and (pair? expr) (%find-pair-location expr)))
            ) ;
        (if from-expr
          from-expr
          (let* ((env (catch #t (lambda () (owlet)) (lambda _ #f)))
                 (data
                   (and env (catch #t (lambda () (env 'error-data)) (lambda _ #f)))
                 ) ;data
                 (type
                   (and env (catch #t (lambda () (env 'error-type)) (lambda _ #f)))
                 ) ;type
                 (matched?
                   (and env
                     data
                     (or (equal? data info)
                       (and (pair? data) (equal? (car data) fmt-args))
                       (equal? data fmt-args)
                     ) ;or
                   ) ;and
                 ) ;matched?
                 (file (and matched? (eq? type 'unbound-variable) (env 'error-file)))
                 (line (and matched? (eq? type 'unbound-variable) (env 'error-line)))
                ) ;
            (if (and (string? file) (integer? line) (> line 0)) (cons file line) #f)
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (format-error-location loc)
      (if (not (pair? loc))
        ""
        (string-append "\n    at " (car loc) ":" (number->string (cdr loc)))
      ) ;if
    ) ;define

    (define (extract-unbound-variable tag info)
      (let ((fmt-args (%normalize-fmt-args info)))
        (cond
         ((and (eq? tag 'unbound-variable)
            (pair? fmt-args)
            (pair? (cdr fmt-args))
            (symbol? (cadr fmt-args))
          ) ;and
          (symbol->string (cadr fmt-args))
         ) ;
         ((and (eq? tag 'unbound-variable)
            (pair? fmt-args)
            (pair? (cdr fmt-args))
            (string? (cadr fmt-args))
          ) ;and
          (cadr fmt-args)
         ) ;
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (unbound-variable-hint var-name)
      (if (not (string? var-name))
        ""
        (let ((libs (catch #t (lambda () (g_function-libraries var-name)) (lambda _ '()))))
          (cond ((null? libs) "")
                ((= (length libs) 1)
                 (string-append "\nHint: `"
                   var-name
                   "` exists in library:\n"
                   "  (import "
                   (object->string (car libs))
                   ")\n"
                   "Please import that library first."
                 ) ;string-append
                ) ;
                (else
                  (string-append "\nHint: `"
                    var-name
                    "` exists in multiple visible libraries:\n"
                    (apply string-append
                      (map (lambda (lib) (string-append "  (import " (object->string lib) ")\n"))
                        libs
                      ) ;map
                    ) ;apply
                    "Please import the appropriate library first."
                  ) ;string-append
                ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (catch-error->string tag info)
      (let* ((fmt-args (%normalize-fmt-args info))
             (base-msg
               (cond
                 (fmt-args (catch #t (lambda () (apply format #f fmt-args)) (lambda _ (car fmt-args)))
                 ) ;fmt-args
                 ((string? info) info)
                 ((not tag) (object->string info))
                 (else (object->string (cons tag info)))
               ) ;cond
             ) ;base-msg
             (loc-str (format-error-location (extract-error-location info)))
             (var-name (extract-unbound-variable tag info))
             (hint (if var-name (unbound-variable-hint var-name) ""))
            ) ;
        (string-append base-msg loc-str hint)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
