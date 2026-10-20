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

(define-library (liii golddoc cli)
  (import (scheme base)
    (scheme eval)
    (liii golddoc args)
    (liii golddoc fuzzy)
    (liii golddoc function)
    (liii golddoc index)
    (liii golddoc index-build)
    (liii golddoc library)
    (liii golddoc render)
    (liii path)
    (liii string)
    (liii sys)
  ) ;import
  (export run-golddoc)
  (begin

    (define (handle-build-json)
      (let ((built-paths (build-function-indexes!)))
        (if (null? built-paths)
          (begin
            (stderr-line "Error: no buildable tests roots found in *load-path*.")
            1
          ) ;begin
          (begin
            (for-each (lambda (built-path)
                        (display "Built function index: ")
                        (display built-path)
                        (newline)
                      ) ;lambda
              built-paths
            ) ;for-each
            0
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    (define (handle-library query)
      (let* ((parts (parse-library-query query))
             (group (and parts (car parts)))
             (doc-path (library-doc-path query))
             (visible-library-root (and parts (find-visible-library-root query)))
            ) ;
        (cond ((not parts) (display-usage) 1)
              ((excluded-test-group? group)
               (stderr-line (string-append "Error: documentation for tests/" group " is not supported yet.")
               ) ;stderr-line
               1
              ) ;
              (doc-path (display (path-read-text doc-path)) 0)
              ((not visible-library-root)
               (let ((fallback-libraries (visible-libraries-for-function query)))
                 (if (null? fallback-libraries)
                   (if (null? (find-function-index-paths))
                     (begin
                       (stderr-line (string-append "Error: function index not found for query: " query)
                       ) ;stderr-line
                       (display-build-json-hint)
                       1
                     ) ;begin
                     (let ((suggestions (suggest-visible-functions query)))
                       (if (null? suggestions)
                         (begin
                           (stderr-line (string-append "Error: library not found in *load-path*: " query))
                           1
                         ) ;begin
                         (begin
                           (display-function-suggestions query suggestions)
                           1
                         ) ;begin
                       ) ;if
                     ) ;let
                   ) ;if
                   (run-function-query query)
                 ) ;if
               ) ;let
              ) ;
              (else (display-library-without-docs query) 1)
        ) ;cond
      ) ;let*
    ) ;define

    (define (handle-library-function library-query exported-name)
      (let* ((parts (parse-library-query library-query))
             (group (and parts (car parts)))
             (doc-path (function-doc-path library-query exported-name))
            ) ;
        (cond ((not parts) (display-usage) 1)
              ((excluded-test-group? group)
               (stderr-line (string-append "Error: documentation for tests/" group " is not supported yet.")
               ) ;stderr-line
               1
              ) ;
              (doc-path (display (path-read-text doc-path)) 0)
              ((not (find-visible-library-root library-query))
               (stderr-line (string-append "Error: library not found in *load-path*: " library-query)
               ) ;stderr-line
               1
              ) ;
              (else
                (if (member library-query (visible-libraries-for-function exported-name))
                  (begin
                    (display-exported-without-docs exported-name (list library-query))
                    1
                  ) ;begin
                  (let ((suggestions (suggest-library-functions library-query exported-name)))
                    (if (null? suggestions)
                      (begin
                        (stderr-line (string-append "Error: documentation file not found for function: "
                                       exported-name " in library: "
                                       library-query
                                     ) ;string-append
                        ) ;stderr-line
                        1
                      ) ;begin
                      (begin
                        (display-library-function-suggestions library-query exported-name suggestions)
                        1
                      ) ;begin
                    ) ;if
                  ) ;let
                ) ;if
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (handle-function function-name)
      (run-function-query function-name)
    ) ;define

    (define (documented-library-queries function-name library-queries)
      (let loop
        ((remaining library-queries) (documented '()))
        (if (null? remaining)
          documented
          (let ((library-query (car remaining)))
            (loop (cdr remaining)
              (if (function-doc-path library-query function-name)
                (append documented (list library-query))
                documented
              ) ;if
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (library-query->import-set library-query)
      ;; 将 "liii/base" 转换为 (liii base) 形式的 import set
      (let ((parts (parse-library-query library-query)))
        (if parts (map string->symbol parts) #f)
      ) ;let
    ) ;define

    (define (display-doc-with-source library-query function-name)
      (let ((doc-path (function-doc-path library-query function-name)))
        ;; 显示源代码（如果有的话）
        (let ((import-set (library-query->import-set library-query)))
          (when import-set
            (let ((env (environment import-set)))
              (let ((func (eval (string->symbol function-name) env)))
                ;; 使用 guard 捕获 procedure-source 的错误
                ;; 某些语法形式（如 define*）会导致 procedure-source 报错
                (let ((source (guard (ex (else #f)) (procedure-source func))))
                  (when (and source (not (null? source)))
                    (display ";; 源代码:")
                    (newline)
                    (write source)
                    (newline)
                    (newline)
                  ) ;when
                ) ;let
              ) ;let
            ) ;let
          ) ;when
        ) ;let
        (display (path-read-text doc-path))
        (newline)
        (display ";; 来自: gf doc ")
        (display library-query)
        (display " ")
        (write function-name)
        (newline)
      ) ;let
    ) ;define

    (define (run-function-query function-name)
      (let ((library-queries (visible-libraries-for-function function-name)))
        (cond
         ((null? library-queries)
          (let ((suggestions (suggest-visible-functions function-name)))
            (if (null? suggestions)
              (begin
                (stderr-line (string-append "Error: function not found in *load-path*: " function-name)
                ) ;stderr-line
                (if (null? (find-function-index-paths)) (display-build-json-hint) #f)
                1
              ) ;begin
              (begin
                (display-function-suggestions function-name suggestions)
                1
              ) ;begin
            ) ;if
          ) ;let
         ) ;
         (else
           (let ((documented-queries (documented-library-queries function-name library-queries))
                ) ;
             (cond ((null? documented-queries)
                    (display-exported-without-docs function-name library-queries)
                    1
                   ) ;
                   ((null? (cdr documented-queries))
                    (display-doc-with-source (car documented-queries) function-name)
                    0
                   ) ;
                   (else (display-doc-with-source (car documented-queries) function-name)
                     (newline)
                     (display ";; 该函数在其他库中也有实现:" (current-error-port))
                     (newline (current-error-port))
                     (for-each (lambda (library-query)
                                 (display ";;   gf doc " (current-error-port))
                                 (display library-query (current-error-port))
                                 (display " " (current-error-port))
                                 (write function-name (current-error-port))
                                 (newline (current-error-port))
                               ) ;lambda
                       (cdr documented-queries)
                     ) ;for-each
                     0
                   ) ;else
             ) ;cond
           ) ;let
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (run-golddoc)
      (let ((parsed (parse-doc-args (argv))))
        (case (car parsed)
              ((help) (display-usage) 0)
              ((build-json) (handle-build-json))
              ((library) (handle-library (cadr parsed)))
              ((library-function) (handle-library-function (cadr parsed) (caddr parsed)))
              ((function) (handle-function (cadr parsed)))
              (else (display-usage) 0)
        ) ;case
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
