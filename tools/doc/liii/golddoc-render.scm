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

(define-library (liii golddoc-render)
  (import (scheme base) (liii golddoc-args) (liii path) (liii string) (liii sys))
  (export stderr-line golddoc-command-name shell-double-quote
    function-doc-command library-doc-command library-source-command
    library-query->display-name library-function-doc-command display-usage
    display-build-json-hint display-function-suggestions
    display-library-function-suggestions display-library-choices
    display-exported-without-docs display-library-without-docs
  ) ;export
  (begin

    (define (stderr-line message)
      (display message (current-error-port))
      (newline (current-error-port))
    ) ;define

    (define (golddoc-command-name)
      (let ((name (path-name (executable))))
        (if (= (string-length name) 0) "gf" name)
      ) ;let
    ) ;define

    (define (shell-double-quote value)
      (let loop
        ((chars (string->list value)) (parts '()))
        (if (null? chars)
          (string-append "\"" (apply string-append (reverse parts)) "\"")
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
      (string-append (golddoc-command-name) " doc " library-query)
    ) ;define

    (define (library-source-command library-query)
      (string-append (golddoc-command-name) " source " library-query)
    ) ;define

    (define (library-query->display-name library-query)
      (let ((parts (parse-library-query library-query)))
        (if parts (string-append "(" (string-join parts " ") ")") library-query)
      ) ;let
    ) ;define

    (define (library-function-doc-command library-query function-name)
      (string-append (golddoc-command-name)
        " doc "
        library-query
        " "
        (shell-double-quote function-name)
      ) ;string-append
    ) ;define

    (define (display-usage)
      (display (string-append "Usage:\n" "  gf doc ORG/LIB\n"
                 "  gf doc ORG/LIB \"FUNC\"\n" "  gf doc \"FUNC\"\n"
                 "  gf doc --build-json\n\n" "Examples:\n"
                 "  gf doc liii/path              # 显示 liii/path 库的完整文档\n"
                 "  gf doc liii/path \"path?\"      # 显示 liii/path 库中 path? 函数的文档和用例\n"
                 "  gf doc \"path-join\"            # 在所有可见库中搜索 path-join 函数并显示其文档\n"
                 "  gf doc \"string-spli\"          # 模糊匹配，提示 string-split 等相似函数\n"
                 "  gf doc --build-json           # 重新构建函数索引（首次使用或测试文件变更后执行）\n\n"
                 "Function Name Mapping Rules:\n"
                 "  Special characters in function names are mapped to file names:\n"
                 "    +     -> plus          (e.g., + -> plus)\n"
                 "    -     -> minus         (e.g., - -> minus)\n"
                 "    *     -> star          (e.g., * -> star)\n"
                 "    /     -> slash         (e.g., / -> slash)\n"
                 "    =     -> eq            (e.g., = -> eq)\n"
                 "    <     -> lt            (e.g., < -> lt)\n"
                 "    <=    -> le            (e.g., <= -> le)\n"
                 "    >     -> gt            (e.g., > -> gt)\n"
                 "    >=    -> ge            (e.g., >= -> ge)\n"
                 "    ?     -> -p            (e.g., path? -> path-p)\n"
                 "    !     -> -bang         (e.g., reverse! -> reverse-bang)\n"
                 "    ->    -> -to-          (e.g., list->vector -> list-to-vector)\n"
                 "    /     -> -slash-       (e.g., path/join -> path-slash-join)\n"
                 "    *     -> -star         (e.g., char* -> char-star)\n"
                 "    =     -> -eq           (e.g., char= -> char-eq)\n"
                 "    <     -> -lt           (e.g., char< -> char-lt)\n"
                 "    >     -> -gt           (e.g., char> -> char-gt)\n"
                 "    <=    -> -le           (e.g., char<= -> char-le)\n"
                 "    >=    -> -ge           (e.g., char>= -> char-ge)\n\n"
                 "How It Works:\n"
                 "  1. gf doc extracts documentation from test files (tests/**/FUNC-test.scm)\n"
                 "  2. Function names are mapped to file names using the rules above\n"
                 "  3. --build-json scans *load-path* to build function-library-index.json\n"
                 "  4. Fuzzy matching uses Levenshtein distance to suggest similar functions\n"
               ) ;string-append
        (current-output-port)
      ) ;display
    ) ;define

    (define (display-build-json-hint)
      (stderr-line "Hint: run `gf doc --build-json` to build function index.")
    ) ;define

    (define (display-function-suggestions function-name suggestions)
      (let ((port (current-error-port)))
        (display (string-append "No exact match for function: " function-name) port)
        (newline port)
        (display "Try one of these commands:" port)
        (newline port)
        (for-each (lambda (suggestion)
                    (display "  " port)
                    (display (function-doc-command suggestion) port)
                    (newline port)
                  ) ;lambda
          suggestions
        ) ;for-each
      ) ;let
    ) ;define

    (define (display-library-function-suggestions library-query function-name suggestions)
      (let ((port (current-error-port)))
        (display (string-append "No exact match for function: " function-name
                   " in library: " library-query
                 ) ;string-append
          port
        ) ;display
        (newline port)
        (display "Try one of these commands:" port)
        (newline port)
        (for-each (lambda (suggestion)
                    (display "  " port)
                    (display (library-function-doc-command library-query suggestion) port)
                    (newline port)
                  ) ;lambda
          suggestions
        ) ;for-each
      ) ;let
    ) ;define

    (define (display-library-choices function-name library-queries)
      (let ((port (current-error-port)))
        (display (string-append "Function is implemented in multiple visible libraries: "
                   function-name
                 ) ;string-append
          port
        ) ;display
        (newline port)
        (display "Try one of these commands:" port)
        (newline port)
        (for-each (lambda (library-query)
                    (display "  " port)
                    (display (library-function-doc-command library-query function-name) port)
                    (newline port)
                  ) ;lambda
          library-queries
        ) ;for-each
      ) ;let
    ) ;define

    (define (display-exported-without-docs function-name library-queries)
      (let ((port (current-error-port)))
        (display (string-append "Function " function-name " is exported in:") port)
        (newline port)
        (for-each (lambda (library-query)
                    (display "  " port)
                    (display (library-query->display-name library-query) port)
                    (newline port)
                  ) ;lambda
          library-queries
        ) ;for-each
        (display "No documentation and test cases available." port)
        (newline port)
        (display "Try one of these commands:" port)
        (newline port)
        (for-each (lambda (library-query)
                    (display "  " port)
                    (display (library-doc-command library-query) port)
                    (newline port)
                    (display "  " port)
                    (display (library-source-command library-query) port)
                    (newline port)
                  ) ;lambda
          library-queries
        ) ;for-each
      ) ;let
    ) ;define

    (define (display-library-without-docs library-query)
      (let ((port (current-error-port)))
        (display (string-append "Library "
                   (library-query->display-name library-query)
                   " exists."
                 ) ;string-append
          port
        ) ;display
        (newline port)
        (display "No documentation and test cases available." port)
        (newline port)
        (display "Try one of these commands:" port)
        (newline port)
        (display "  " port)
        (display (library-source-command library-query) port)
        (newline port)
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
