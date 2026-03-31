;
; Copyright (C) 2026 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(define-library (liii golddoc-cli)
  (import (scheme base)
          (liii golddoc-args)
          (liii golddoc-fuzzy)
          (liii golddoc-function)
          (liii golddoc-index)
          (liii golddoc-index-build)
          (liii golddoc-library)
          (liii path)
          (liii sys)
  ) ;import
  (export run-golddoc)
  (begin

    (define (stderr-line message)
      (display message (current-error-port))
      (newline (current-error-port))
    ) ;define

    (define (display-usage)
      (let ((port (current-output-port)))
        (display "Usage:" port) (newline port)
        (display "  gf doc ORG/LIB" port) (newline port)
        (display "  gf doc ORG/LIB FUNC" port) (newline port)
        (display "  gf doc FUNC" port) (newline port)
        (display "  gf doc --build-json" port) (newline port)
        (newline port)
        (display "Examples:" port) (newline port)
        (display "  gf doc liii/path              # 显示 liii/path 库的完整文档" port) (newline port)
        (display "  gf doc liii/path \"path?\"      # 显示 liii/path 库中 path? 函数的文档和用例" port) (newline port)
        (display "  gf doc \"path-join\"            # 在所有可见库中搜索 path-join 函数并显示其文档" port) (newline port)
        (display "  gf doc \"string-spli\"          # 模糊匹配，提示 string-split 等相似函数" port) (newline port)
        (display "  gf doc --build-json           # 重新构建函数索引（首次使用或测试文件变更后执行）" port) (newline port)
      ) ;let
    ) ;define

    (define (display-build-json-hint)
      (stderr-line "Hint: run `gf doc --build-json` to build function index.")
    ) ;define

    (define (display-function-suggestions function-name suggestions)
      (let ((port (current-error-port)))
        (display (string-append "No exact match for function: " function-name) port)
        (newline port)
        (display "Did you mean:" port)
        (newline port)
        (for-each
          (lambda (suggestion)
            (display "  " port)
            (display suggestion port)
            (newline port)
          ) ;lambda
          suggestions
        ) ;for-each
      ) ;let
    ) ;define

    (define (display-library-function-suggestions library-query function-name suggestions)
      (let ((port (current-error-port)))
        (display (string-append "No exact match for function: "
                                function-name
                                " in library: "
                                library-query)
                 port)
        (newline port)
        (display "Did you mean:" port)
        (newline port)
        (for-each
          (lambda (suggestion)
            (display "  " port)
            (display suggestion port)
            (newline port)
          ) ;lambda
          suggestions
        ) ;for-each
      ) ;let
    ) ;define

    (define (display-library-choices function-name library-queries)
      (let ((port (current-error-port)))
        (display (string-append "Function is implemented in multiple visible libraries: " function-name) port)
        (newline port)
        (for-each
          (lambda (library-query)
            (display "  " port)
            (display library-query port)
            (newline port)
          ) ;lambda
          library-queries
        ) ;for-each
      ) ;let
    ) ;define

    (define (run-function-query function-name)
      (let ((library-queries (visible-libraries-for-function function-name)))
        (cond
          ((null? library-queries)
           (let ((suggestions (suggest-visible-functions function-name)))
             (if (null? suggestions)
                 (begin
                   (stderr-line (string-append "Error: function not found in *load-path*: " function-name))
                   (if (null? (find-function-index-paths))
                       (display-build-json-hint)
                       #f
                   ) ;if
                   1
                 ) ;begin
                 (begin
                   (display-function-suggestions function-name suggestions)
                   1
                 ) ;begin
             ) ;if
           ) ;let
          ) ;
          ((null? (cdr library-queries))
           (let ((doc-path (function-doc-path (car library-queries) function-name)))
             (if doc-path
                 (begin
                   (display (path-read-text doc-path))
                   0
                 ) ;begin
                 (begin
                   (stderr-line (string-append "Error: documentation file not found for function: " function-name))
                   1
                 ) ;begin
             ) ;if
           ) ;let
          ) ;
          (else
           (display-library-choices function-name library-queries)
           1
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (run-golddoc)
      (let ((parsed (parse-doc-args (argv))))
        (case (car parsed)
          ((build-json)
           (let ((built-paths (build-function-indexes!)))
             (if (null? built-paths)
                 (begin
                   (stderr-line "Error: no buildable tests roots found in *load-path*.")
                   1
                 ) ;begin
                 (begin
                   (for-each
                     (lambda (built-path)
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
          ) ;
          ((library)
           (let* ((query (cadr parsed))
                  (parts (parse-library-query query))
                  (group (and parts (car parts)))
                  (doc-path (library-doc-path query))
                  (visible-library-root (and parts
                                             (find-visible-library-root query))))
             (cond
               ((not parts)
                (display-usage)
                1
               ) ;
               ((excluded-test-group? group)
                (stderr-line (string-append "Error: documentation for tests/" group " is not supported yet."))
                1
               ) ;
               (doc-path
                (display (path-read-text doc-path))
                0
               ) ;doc-path
               ((not visible-library-root)
                (let ((fallback-libraries (visible-libraries-for-function query)))
                  (if (null? fallback-libraries)
                      (if (null? (find-function-index-paths))
                          (begin
                            (stderr-line (string-append "Error: function index not found for query: " query))
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
               (else
                (stderr-line (string-append "Error: documentation file not found for library: " query))
                1
               ) ;else
             ) ;cond
           ) ;let*
          ) ;
          ((library-function)
           (let* ((library-query (cadr parsed))
                  (exported-name (caddr parsed))
                  (parts (parse-library-query library-query))
                  (group (and parts (car parts)))
                  (doc-path (function-doc-path library-query exported-name)))
             (cond
               ((not parts)
                (display-usage)
                1
               ) ;
               ((excluded-test-group? group)
                (stderr-line (string-append "Error: documentation for tests/" group " is not supported yet."))
                1
               ) ;
               (doc-path
                (display (path-read-text doc-path))
                0
               ) ;doc-path
               ((not (find-visible-library-root library-query))
                (stderr-line (string-append "Error: library not found in *load-path*: " library-query))
                1
               ) ;
               (else
                (let ((suggestions (suggest-library-functions library-query exported-name)))
                  (if (null? suggestions)
                      (begin
                        (stderr-line (string-append "Error: documentation file not found for function: "
                                                    exported-name
                                                    " in library: "
                                                    library-query)
                        ) ;stderr-line
                        1
                      ) ;begin
                      (begin
                        (display-library-function-suggestions library-query exported-name suggestions)
                        1
                      ) ;begin
                  ) ;if
                ) ;let
               ) ;else
             ) ;cond
          ) ;let*
          ) ;
          ((function)
           (run-function-query (cadr parsed))
          ) ;
          (else
           (display-usage)
           0
          ) ;else
        ) ;case
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
