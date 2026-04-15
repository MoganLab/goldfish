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

(define-library (liii golddoc-cli)
  (import (scheme base)
    (scheme eval)
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

    (define (golddoc-command-name)
      (let ((name (path-name (executable))))
        (if (= (string-length name) 0)
          "gf"
          name
        ) ;if
      ) ;let
    ) ;define

    (define (shell-double-quote value)
      (let loop
        ((chars (string->list value))
         (parts '())
        ) ;
        (if (null? chars)
          (string-append "\""
            (apply string-append (reverse parts))
            "\""
          ) ;string-append
          (let ((ch (car chars)))
            (loop (cdr chars)
              (cond ((char=? ch #\\) (cons "\\\\" parts))
                    ((char=? ch #\") (cons "\\\"" parts))
                    ((char=? ch #\$) (cons "\\$" parts))
                    ((char=? ch #\`) (cons "\\`" parts))
                    (else (cons (string ch) parts))
              ) ;cond
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (function-doc-command function-name)
      (string-append (golddoc-command-name)
        " doc "
        (shell-double-quote function-name)
      ) ;string-append
    ) ;define

    (define (library-doc-command library-query)
      (string-append (golddoc-command-name)
        " doc "
        library-query
      ) ;string-append
    ) ;define

    (define (library-source-command library-query)
      (string-append (golddoc-command-name)
        " source "
        library-query
      ) ;string-append
    ) ;define

    (define (library-query->display-name library-query
            ) ;library-query->display-name
      (let ((parts (parse-library-query library-query)
            ) ;parts
           ) ;
        (if parts
          (string-append "("
            (car parts)
            " "
            (cdr parts)
            ")"
          ) ;string-append
          library-query
        ) ;if
      ) ;let
    ) ;define

    (define (library-function-doc-command library-query
              function-name
            ) ;library-function-doc-command
      (string-append (golddoc-command-name)
        " doc "
        library-query
        " "
        (shell-double-quote function-name)
      ) ;string-append
    ) ;define

    (define (display-usage)
      (let ((port (current-output-port)))
        (display "Usage:" port)
        (newline port)
        (display "  gf doc ORG/LIB" port)
        (newline port)
        (display "  gf doc ORG/LIB \"FUNC\""
          port
        ) ;display
        (newline port)
        (display "  gf doc \"FUNC\"" port)
        (newline port)
        (display "  gf doc --build-json" port)
        (newline port)
        (newline port)
        (display "Examples:" port)
        (newline port)
        (display "  gf doc liii/path              # 显示 liii/path 库的完整文档"
          port
        ) ;display
        (newline port)
        (display "  gf doc liii/path \"path?\"      # 显示 liii/path 库中 path? 函数的文档和用例"
          port
        ) ;display
        (newline port)
        (display "  gf doc \"path-join\"            # 在所有可见库中搜索 path-join 函数并显示其文档"
          port
        ) ;display
        (newline port)
        (display "  gf doc \"string-spli\"          # 模糊匹配，提示 string-split 等相似函数"
          port
        ) ;display
        (newline port)
        (display "  gf doc --build-json           # 重新构建函数索引（首次使用或测试文件变更后执行）"
          port
        ) ;display
        (newline port)
        (newline port)
        (display "Function Name Mapping Rules:"
          port
        ) ;display
        (newline port)
        (display "  Special characters in function names are mapped to file names:"
          port
        ) ;display
        (newline port)
        (display "    +     -> plus          (e.g., + -> plus)"
          port
        ) ;display
        (newline port)
        (display "    -     -> minus         (e.g., - -> minus)"
          port
        ) ;display
        (newline port)
        (display "    *     -> star          (e.g., * -> star)"
          port
        ) ;display
        (newline port)
        (display "    /     -> slash         (e.g., / -> slash)"
          port
        ) ;display
        (newline port)
        (display "    =     -> eq            (e.g., = -> eq)"
          port
        ) ;display
        (newline port)
        (display "    <     -> lt            (e.g., < -> lt)"
          port
        ) ;display
        (newline port)
        (display "    <=    -> le            (e.g., <= -> le)"
          port
        ) ;display
        (newline port)
        (display "    >     -> gt            (e.g., > -> gt)"
          port
        ) ;display
        (newline port)
        (display "    >=    -> ge            (e.g., >= -> ge)"
          port
        ) ;display
        (newline port)
        (display "    ?     -> -p            (e.g., path? -> path-p)"
          port
        ) ;display
        (newline port)
        (display "    !     -> -bang         (e.g., reverse! -> reverse-bang)"
          port
        ) ;display
        (newline port)
        (display "    ->    -> -to-          (e.g., list->vector -> list-to-vector)"
          port
        ) ;display
        (newline port)
        (display "    /     -> -slash-       (e.g., path/join -> path-slash-join)"
          port
        ) ;display
        (newline port)
        (display "    *     -> -star         (e.g., char* -> char-star)"
          port
        ) ;display
        (newline port)
        (display "    =     -> -eq           (e.g., char= -> char-eq)"
          port
        ) ;display
        (newline port)
        (display "    <     -> -lt           (e.g., char< -> char-lt)"
          port
        ) ;display
        (newline port)
        (display "    >     -> -gt           (e.g., char> -> char-gt)"
          port
        ) ;display
        (newline port)
        (display "    <=    -> -le           (e.g., char<= -> char-le)"
          port
        ) ;display
        (newline port)
        (display "    >=    -> -ge           (e.g., char>= -> char-ge)"
          port
        ) ;display
        (newline port)
        (newline port)
        (display "How It Works:" port)
        (newline port)
        (display "  1. gf doc extracts documentation from test files (tests/**/FUNC-test.scm)"
          port
        ) ;display
        (newline port)
        (display "  2. Function names are mapped to file names using the rules above"
          port
        ) ;display
        (newline port)
        (display "  3. --build-json scans *load-path* to build function-library-index.json"
          port
        ) ;display
        (newline port)
        (display "  4. Fuzzy matching uses Levenshtein distance to suggest similar functions"
          port
        ) ;display
        (newline port)
      ) ;let
    ) ;define

    (define (display-build-json-hint)
      (stderr-line "Hint: run `gf doc --build-json` to build function index."
      ) ;stderr-line
    ) ;define

    (define (display-function-suggestions function-name
              suggestions
            ) ;display-function-suggestions
      (let ((port (current-error-port)))
        (display (string-append "No exact match for function: "
                   function-name
                 ) ;string-append
          port
        ) ;display
        (newline port)
        (display "Try one of these commands:"
          port
        ) ;display
        (newline port)
        (for-each (lambda (suggestion)
                    (display "  " port)
                    (display (function-doc-command suggestion)
                      port
                    ) ;display
                    (newline port)
                  ) ;lambda
          suggestions
        ) ;for-each
      ) ;let
    ) ;define

    (define (display-library-function-suggestions library-query
              function-name
              suggestions
            ) ;display-library-function-suggestions
      (let ((port (current-error-port)))
        (display (string-append "No exact match for function: "
                   function-name
                   " in library: "
                   library-query
                 ) ;string-append
          port
        ) ;display
        (newline port)
        (display "Try one of these commands:"
          port
        ) ;display
        (newline port)
        (for-each (lambda (suggestion)
                    (display "  " port)
                    (display (library-function-doc-command library-query
                               suggestion
                             ) ;library-function-doc-command
                      port
                    ) ;display
                    (newline port)
                  ) ;lambda
          suggestions
        ) ;for-each
      ) ;let
    ) ;define

    (define (display-library-choices function-name
              library-queries
            ) ;display-library-choices
      (let ((port (current-error-port)))
        (display (string-append "Function is implemented in multiple visible libraries: "
                   function-name
                 ) ;string-append
          port
        ) ;display
        (newline port)
        (display "Try one of these commands:"
          port
        ) ;display
        (newline port)
        (for-each (lambda (library-query)
                    (display "  " port)
                    (display (library-function-doc-command library-query
                               function-name
                             ) ;library-function-doc-command
                      port
                    ) ;display
                    (newline port)
                  ) ;lambda
          library-queries
        ) ;for-each
      ) ;let
    ) ;define

    (define (display-exported-without-docs function-name
              library-queries
            ) ;display-exported-without-docs
      (let ((port (current-error-port)))
        (display (string-append "Function "
                   function-name
                   " is exported in:"
                 ) ;string-append
          port
        ) ;display
        (newline port)
        (for-each (lambda (library-query)
                    (display "  " port)
                    (display (library-query->display-name library-query
                             ) ;library-query->display-name
                      port
                    ) ;display
                    (newline port)
                  ) ;lambda
          library-queries
        ) ;for-each
        (display "No documentation and test cases available."
          port
        ) ;display
        (newline port)
        (display "Try one of these commands:"
          port
        ) ;display
        (newline port)
        (for-each (lambda (library-query)
                    (display "  " port)
                    (display (library-doc-command library-query)
                      port
                    ) ;display
                    (newline port)
                    (display "  " port)
                    (display (library-source-command library-query)
                      port
                    ) ;display
                    (newline port)
                  ) ;lambda
          library-queries
        ) ;for-each
      ) ;let
    ) ;define

    (define (display-library-without-docs library-query
            ) ;display-library-without-docs
      (let ((port (current-error-port)))
        (display (string-append "Library "
                   (library-query->display-name library-query
                   ) ;library-query->display-name
                   " exists."
                 ) ;string-append
          port
        ) ;display
        (newline port)
        (display "No documentation and test cases available."
          port
        ) ;display
        (newline port)
        (display "Try one of these commands:"
          port
        ) ;display
        (newline port)
        (display "  " port)
        (display (library-source-command library-query)
          port
        ) ;display
        (newline port)
      ) ;let
    ) ;define

    (define (handle-build-json)
      (let ((built-paths (build-function-indexes!))
           ) ;
        (if (null? built-paths)
          (begin
            (stderr-line "Error: no buildable tests roots found in *load-path*."
            ) ;stderr-line
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
             (visible-library-root (and parts
                                     (find-visible-library-root query)
                                   ) ;and
             ) ;visible-library-root
            ) ;
        (cond ((not parts) (display-usage) 1)
              ((excluded-test-group? group)
               (stderr-line (string-append "Error: documentation for tests/"
                              group
                              " is not supported yet."
                            ) ;string-append
               ) ;stderr-line
               1
              ) ;
              (doc-path (display (path-read-text doc-path))
                0
              ) ;doc-path
              ((not visible-library-root)
               (let ((fallback-libraries (visible-libraries-for-function query)
                     ) ;fallback-libraries
                    ) ;
                 (if (null? fallback-libraries)
                   (if (null? (find-function-index-paths))
                     (begin
                       (stderr-line (string-append "Error: function index not found for query: "
                                      query
                                    ) ;string-append
                       ) ;stderr-line
                       (display-build-json-hint)
                       1
                     ) ;begin
                     (let ((suggestions (suggest-visible-functions query)
                           ) ;suggestions
                          ) ;
                       (if (null? suggestions)
                         (begin
                           (stderr-line (string-append "Error: library not found in *load-path*: "
                                          query
                                        ) ;string-append
                           ) ;stderr-line
                           1
                         ) ;begin
                         (begin
                           (display-function-suggestions query
                             suggestions
                           ) ;display-function-suggestions
                           1
                         ) ;begin
                       ) ;if
                     ) ;let
                   ) ;if
                   (run-function-query query)
                 ) ;if
               ) ;let
              ) ;
              (else (display-library-without-docs query)
                1
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (handle-library-function library-query
              exported-name
            ) ;handle-library-function
      (let* ((parts (parse-library-query library-query)
             ) ;parts
             (group (and parts (car parts)))
             (doc-path (function-doc-path library-query
                         exported-name
                       ) ;function-doc-path
             ) ;doc-path
            ) ;
        (cond ((not parts) (display-usage) 1)
              ((excluded-test-group? group)
               (stderr-line (string-append "Error: documentation for tests/"
                              group
                              " is not supported yet."
                            ) ;string-append
               ) ;stderr-line
               1
              ) ;
              (doc-path (display (path-read-text doc-path))
                0
              ) ;doc-path
              ((not (find-visible-library-root library-query
                    ) ;find-visible-library-root
               ) ;not
               (stderr-line (string-append "Error: library not found in *load-path*: "
                              library-query
                            ) ;string-append
               ) ;stderr-line
               1
              ) ;
              (else (if (member library-query
                          (visible-libraries-for-function exported-name
                          ) ;visible-libraries-for-function
                        ) ;member
                      (begin
                        (display-exported-without-docs exported-name
                          (list library-query)
                        ) ;display-exported-without-docs
                        1
                      ) ;begin
                      (let ((suggestions (suggest-library-functions library-query
                                           exported-name
                                         ) ;suggest-library-functions
                            ) ;suggestions
                           ) ;
                        (if (null? suggestions)
                          (begin
                            (stderr-line (string-append "Error: documentation file not found for function: "
                                           exported-name
                                           " in library: "
                                           library-query
                                         ) ;string-append
                            ) ;stderr-line
                            1
                          ) ;begin
                          (begin
                            (display-library-function-suggestions library-query
                              exported-name
                              suggestions
                            ) ;display-library-function-suggestions
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

    (define (documented-library-queries function-name
              library-queries
            ) ;documented-library-queries
      (let loop
        ((remaining library-queries)
         (documented '())
        ) ;
        (if (null? remaining)
          documented
          (let ((library-query (car remaining)))
            (loop (cdr remaining)
              (if (function-doc-path library-query
                    function-name
                  ) ;function-doc-path
                (append documented (list library-query))
                documented
              ) ;if
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (library-query->import-set library-query
            ) ;library-query->import-set
      ;; 将 "liii/base" 转换为 (liii base) 形式的 import set
      (let ((parts (parse-library-query library-query)
            ) ;parts
           ) ;
        (if parts
          (list (string->symbol (car parts))
            (string->symbol (cdr parts))
          ) ;list
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (display-doc-with-source library-query
              function-name
            ) ;display-doc-with-source
      (let ((doc-path (function-doc-path library-query
                        function-name
                      ) ;function-doc-path
            ) ;doc-path
           ) ;
        ;; 显示源代码（如果有的话）
        (let ((import-set (library-query->import-set library-query
                          ) ;library-query->import-set
              ) ;import-set
             ) ;
          (when import-set
            (let ((env (environment import-set)))
              (let ((func (eval (string->symbol function-name)
                            env
                          ) ;eval
                    ) ;func
                   ) ;
                ;; 使用 guard 捕获 procedure-source 的错误
                ;; 某些语法形式（如 define*）会导致 procedure-source 报错
                (let ((source (guard (ex (else #f))
                                (procedure-source func)
                              ) ;guard
                      ) ;source
                     ) ;
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
      (let ((library-queries (visible-libraries-for-function function-name
                             ) ;visible-libraries-for-function
            ) ;library-queries
           ) ;
        (cond ((null? library-queries)
               (let ((suggestions (suggest-visible-functions function-name
                                  ) ;suggest-visible-functions
                     ) ;suggestions
                    ) ;
                 (if (null? suggestions)
                   (begin
                     (stderr-line (string-append "Error: function not found in *load-path*: "
                                    function-name
                                  ) ;string-append
                     ) ;stderr-line
                     (if (null? (find-function-index-paths))
                       (display-build-json-hint)
                       #f
                     ) ;if
                     1
                   ) ;begin
                   (begin
                     (display-function-suggestions function-name
                       suggestions
                     ) ;display-function-suggestions
                     1
                   ) ;begin
                 ) ;if
               ) ;let
              ) ;
              (else (let ((documented-queries (documented-library-queries function-name
                                                library-queries
                                              ) ;documented-library-queries
                          ) ;documented-queries
                         ) ;
                      (cond ((null? documented-queries)
                             (display-exported-without-docs function-name
                               library-queries
                             ) ;display-exported-without-docs
                             1
                            ) ;
                            ((null? (cdr documented-queries))
                             (display-doc-with-source (car documented-queries)
                               function-name
                             ) ;display-doc-with-source
                             0
                            ) ;
                            (else (display-doc-with-source (car documented-queries)
                                    function-name
                                  ) ;display-doc-with-source
                              (newline)
                              (display ";; 该函数在其他库中也有实现:"
                                (current-error-port)
                              ) ;display
                              (newline (current-error-port))
                              (for-each (lambda (library-query)
                                          (display ";;   gf doc "
                                            (current-error-port)
                                          ) ;display
                                          (display library-query
                                            (current-error-port)
                                          ) ;display
                                          (display " " (current-error-port))
                                          (write function-name
                                            (current-error-port)
                                          ) ;write
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
              ((library)
               (handle-library (cadr parsed))
              ) ;
              ((library-function)
               (handle-library-function (cadr parsed)
                 (caddr parsed)
               ) ;handle-library-function
              ) ;
              ((function)
               (handle-function (cadr parsed))
              ) ;
              (else (display-usage) 0)
        ) ;case
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
