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

(define-library (liii goldsource-cli)
  (import (scheme base)
    (liii goldsource-args)
    (liii goldsource-library)
    (liii path)
    (liii sys)
  ) ;import
  (export run-goldsource)
  (begin

    (define (stderr-line message)
      (display message (current-error-port))
      (newline (current-error-port))
    ) ;define

    (define (display-usage)
      (let ((port (current-output-port)))
        (display "Usage:" port)
        (newline port)
        (display "  gf source ORG/LIB" port)
        (newline port)
        (newline port)
        (display "Examples:" port)
        (newline port)
        (display "  gf source liii/string        # 显示 (liii string) 库的源代码"
          port
        ) ;display
        (newline port)
        (display "  gf source scheme/base        # 显示 (scheme base) 库的源代码"
          port
        ) ;display
        (newline port)
      ) ;let
    ) ;define

    ;; args 为显式的参数列表（字符串列表），供单元测试直接驱动；
    ;; 未给出时从进程命令行 (argv) 解析。
    (define* (run-goldsource (args #f))
      (let* ((full-args
               (cond ((not args) (argv))
                     ((and (pair? args) (string=? (car args) "source")) (cons "gf" args))
                     ((and (pair? args) (member "source" args)) args)
                     (else (cons "gf" (cons "source" args)))
               ) ;cond
             ) ;full-args
             (parsed (parse-source-args full-args))
            ) ;
        (case (car parsed)
              ((help) (display-usage) 0)
              ((library)
               (let* ((query (cadr parsed)) (source-path (source-library-path query)))
                 (if source-path
                   (begin
                     (display (path-read-text source-path))
                     0
                   ) ;begin
                   (begin
                     (stderr-line (string-append "Error: library not found in *load-path*: " query))
                     1
                   ) ;begin
                 ) ;if
               ) ;let*
              ) ;
              (else (display-usage) 1)
        ) ;case
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
