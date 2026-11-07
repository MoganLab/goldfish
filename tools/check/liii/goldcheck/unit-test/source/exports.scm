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

;; 库导出名提取：仅关心 (export ...) 声明的对外名字，不区分定义与重导出。

(define-library (liii goldcheck unit-test source exports)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii path)
    (liii goldcheck unit-test source parse)
  ) ;import
  (export extract-library-exports
    extract-library-defined-exports
    extract-library-reexported-entries
  ) ;export
  (begin

    (define (extract-decl-exports decl exports)
      (if (and (pair? decl) (eq? (car decl) 'export))
        (let loop
          ((items (cdr decl)) (acc exports))
          (if (null? items)
            acc
            (let ((item (car items)))
              (cond
               ((symbol? item) (loop (cdr items) (cons (symbol->string item) acc)))
               ((and (pair? item) (eq? (car item) 'rename) (= (length item) 3))
                (let ((ext (caddr item)))
                  (loop (cdr items) (cons (symbol->string ext) acc))
                ) ;let
               ) ;
               (else (loop (cdr items) acc))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
        exports
      ) ;if
    ) ;define

    (define (extract-exports-from-port port)
      (let loop
        ()
        (let ((expr (catch #t (lambda () (read port)) (lambda (k e) (eof-object)))))
          (cond ((eof-object? expr) '())
                ((and (pair? expr) (eq? (car expr) 'define-library))
                 (let decl-loop
                   ((rem (cddr expr)) (exports '()))
                   (if (null? rem)
                     (reverse exports)
                     (decl-loop (cdr rem) (extract-decl-exports (car rem) exports))
                   ) ;if
                 ) ;let
                ) ;
                (else (loop))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (extract-library-exports source-path)
      (call-with-input-file (resolve-repo-file source-path) extract-exports-from-port)
    ) ;define

    (define (filter-library-exports source-path keep-defined?)
      (let ((struct (parse-library-structure source-path)))
        (if (not struct)
          '()
          (let ((exports (cdr (assq 'exports struct))) (defs (cdr (assq 'defs struct))))
            (let loop
              ((rem exports) (acc '()))
              (if (null? rem)
                (reverse acc)
                (let* ((entry (car rem))
                       (ext-name (car entry))
                       (int-name (cdr entry))
                       (defined? (member int-name defs string=?))
                      ) ;
                  (cond ((and keep-defined? defined?) (loop (cdr rem) (cons ext-name acc)))
                        ((and (not keep-defined?) (not defined?)) (loop (cdr rem) (cons entry acc)))
                        (else (loop (cdr rem) acc))
                  ) ;cond
                ) ;let*
              ) ;if
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (extract-library-defined-exports source-path)
      (filter-library-exports source-path #t)
    ) ;define

    (define (extract-library-reexported-entries source-path)
      (filter-library-exports source-path #f)
    ) ;define

  ) ;begin
) ;define-library
