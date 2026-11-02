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
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii goldjson)
  (import (scheme base)
    (scheme write)
    (scheme process-context)
    (liii base)
    (liii list)
    (liii json)
    (liii string)
    (liii path)
  ) ;import
  (export main run-goldjson parse-json-args display-json-help)
  (begin

    (define (display-json-help port)
      (display "Usage: gf json [OPTIONS] [INFILE] [OUTFILE]\n\n" port)
      (display "Validate, format, or process JSON from INFILE (or stdin) and write to OUTFILE (or stdout).\n\n"
        port
      ) ;display
      (display "Options:\n" port)
      (display "  -i, --indent <N>      Set number of spaces for indentation (default: 2)\n"
        port
      ) ;display
      (display "      --tab             Use tabs for indentation instead of spaces\n"
        port
      ) ;display
      (display "  -c, --compact         Compact JSON onto a single line (no indent)\n"
        port
      ) ;display
      (display "  -s, --sort-keys       Sort object keys alphabetically\n" port)
      (display "      --ensure-ascii    Escape non-ASCII characters as \\uXXXX\n"
        port
      ) ;display
      (display "      --no-ensure-ascii Direct UTF-8 characters output (default)\n"
        port
      ) ;display
      (display "      --json-lines      Parse and format each line as an independent JSON object\n"
        port
      ) ;display
      (display "  -v, --validate        Validate JSON only without formatting output\n"
        port
      ) ;display
      (display "  -h, --help            Display this help and exit\n\n" port)
      (display "Arguments:\n" port)
      (display "  INFILE                Input file path (default or '-': stdin)\n"
        port
      ) ;display
      (display "  OUTFILE               Output file path (default or '-': stdout)\n"
        port
      ) ;display
    ) ;define

    (define (%read-all-port p)
      (let loop
        ((acc '()))
        (let ((chunk (read-string 8192 p)))
          (if (or (eof-object? chunk) (zero? (string-length chunk)))
            (apply string-append (reverse acc))
            (loop (cons chunk acc))
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (parse-json-args args)
      (let loop
        ((rem args)
         (indent 2)
         (compact #f)
         (sort-keys #f)
         (ensure-ascii #f)
         (json-lines #f)
         (validate #f)
         (help #f)
         (positionals '())
        ) ;
        (if (null? rem)
          (list (cons 'indent (if compact #f indent))
            (cons 'compact compact)
            (cons 'sort-keys sort-keys)
            (cons 'ensure-ascii ensure-ascii)
            (cons 'json-lines json-lines)
            (cons 'validate validate)
            (cons 'help help)
            (cons 'positionals (reverse positionals))
          ) ;list
          (let ((arg (car rem)))
            (cond ((or (string=? arg "-h") (string=? arg "--help"))
                   (loop (cdr rem)
                     indent
                     compact
                     sort-keys
                     ensure-ascii
                     json-lines
                     validate
                     #t
                     positionals
                   ) ;loop
                  ) ;
                  ((or (string=? arg "-c") (string=? arg "--compact"))
                   (loop (cdr rem)
                     indent
                     #t
                     sort-keys
                     ensure-ascii
                     json-lines
                     validate
                     help
                     positionals
                   ) ;loop
                  ) ;
                  ((string=? arg "--tab")
                   (loop (cdr rem)
                     "\t"
                     #f
                     sort-keys
                     ensure-ascii
                     json-lines
                     validate
                     help
                     positionals
                   ) ;loop
                  ) ;
                  ((or (string=? arg "-s") (string=? arg "--sort-keys"))
                   (loop (cdr rem)
                     indent
                     compact
                     #t
                     ensure-ascii
                     json-lines
                     validate
                     help
                     positionals
                   ) ;loop
                  ) ;
                  ((string=? arg "--ensure-ascii")
                   (loop (cdr rem)
                     indent
                     compact
                     sort-keys
                     #t
                     json-lines
                     validate
                     help
                     positionals
                   ) ;loop
                  ) ;
                  ((string=? arg "--no-ensure-ascii")
                   (loop (cdr rem)
                     indent
                     compact
                     sort-keys
                     #f
                     json-lines
                     validate
                     help
                     positionals
                   ) ;loop
                  ) ;
                  ((string=? arg "--json-lines")
                   (loop (cdr rem)
                     indent
                     compact
                     sort-keys
                     ensure-ascii
                     #t
                     validate
                     help
                     positionals
                   ) ;loop
                  ) ;
                  ((or (string=? arg "-v") (string=? arg "--validate"))
                   (loop (cdr rem)
                     indent
                     compact
                     sort-keys
                     ensure-ascii
                     json-lines
                     #t
                     help
                     positionals
                   ) ;loop
                  ) ;
                  ((or (string=? arg "-i") (string=? arg "--indent"))
                   (if (null? (cdr rem))
                     (begin
                       (display "gf json: error: --indent requires an argument\n" (current-error-port))
                       #f
                     ) ;begin
                     (let ((n (string->number (cadr rem))))
                       (if (and (integer? n) (>= n 0))
                         (loop (cddr rem)
                           n
                           #f
                           sort-keys
                           ensure-ascii
                           json-lines
                           validate
                           help
                           positionals
                         ) ;loop
                         (begin
                           (display "gf json: error: --indent requires a non-negative integer\n"
                             (current-error-port)
                           ) ;display
                           #f
                         ) ;begin
                       ) ;if
                     ) ;let
                   ) ;if
                  ) ;
                  ((string-starts? arg "--indent=")
                   (let* ((val-str (substring arg 9 (string-length arg))) (n (string->number val-str)))
                     (if (and (integer? n) (>= n 0))
                       (loop (cdr rem)
                         n
                         #f
                         sort-keys
                         ensure-ascii
                         json-lines
                         validate
                         help
                         positionals
                       ) ;loop
                       (begin
                         (display "gf json: error: --indent requires a non-negative integer\n"
                           (current-error-port)
                         ) ;display
                         #f
                       ) ;begin
                     ) ;if
                   ) ;let*
                  ) ;
                  ((string-starts? arg "-")
                   (if (string=? arg "-")
                     (loop (cdr rem)
                       indent
                       compact
                       sort-keys
                       ensure-ascii
                       json-lines
                       validate
                       help
                       (cons arg positionals)
                     ) ;loop
                     (begin
                       (display (string-append "gf json: unrecognized option: " arg "\n")
                         (current-error-port)
                       ) ;display
                       #f
                     ) ;begin
                   ) ;if
                  ) ;
                  (else (loop (cdr rem)
                          indent
                          compact
                          sort-keys
                          ensure-ascii
                          json-lines
                          validate
                          help
                          (cons arg positionals)
                        ) ;loop
                  ) ;else
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (run-goldjson args)
      (let ((parsed (parse-json-args args)))
        (if (not parsed)
          1
          (let* ((help (cdr (assq 'help parsed)))
                 (indent (cdr (assq 'indent parsed)))
                 (compact (cdr (assq 'compact parsed)))
                 (sort-keys (cdr (assq 'sort-keys parsed)))
                 (ensure-ascii (cdr (assq 'ensure-ascii parsed)))
                 (json-lines (cdr (assq 'json-lines parsed)))
                 (validate (cdr (assq 'validate parsed)))
                 (positionals (cdr (assq 'positionals parsed)))
                ) ;
            (if help
              (begin
                (display-json-help (current-output-port))
                0
              ) ;begin
              (if (> (length positionals) 2)
                (begin
                  (display "gf json: too many arguments\n" (current-error-port))
                  1
                ) ;begin
                (let* ((in-file
                         (and (pair? positionals)
                           (not (string=? (car positionals) "-"))
                           (car positionals)
                         ) ;and
                       ) ;in-file
                       (out-file
                         (and (> (length positionals) 1)
                           (not (string=? (cadr positionals) "-"))
                           (cadr positionals)
                         ) ;and
                       ) ;out-file
                      ) ;
                  (%process-json in-file out-file indent compact sort-keys
                    ensure-ascii json-lines validate
                  ) ;%process-json
                ) ;let*
              ) ;if
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%process-json in-file out-file indent compact sort-keys
              ensure-ascii json-lines validate
            ) ;%process-json
      (let* ((in-thunk
               (lambda (in-port)
                 (if out-file
                   (call-with-output-file out-file
                     (lambda (out-port)
                       (%run-conversion in-port out-port indent compact
                         sort-keys ensure-ascii json-lines validate
                       ) ;%run-conversion
                     ) ;lambda
                   ) ;call-with-output-file
                   (%run-conversion in-port
                     (current-output-port)
                     indent
                     compact
                     sort-keys
                     ensure-ascii
                     json-lines
                     validate
                   ) ;%run-conversion
                 ) ;if
               ) ;lambda
             ) ;in-thunk
            ) ;
        (if in-file
          (catch #t
            (lambda () (call-with-input-file in-file in-thunk))
            (lambda (tag err)
              (display (string-append "gf json: error: could not open file: " in-file "\n")
                (current-error-port)
              ) ;display
              1
            ) ;lambda
          ) ;catch
          (in-thunk (current-input-port))
        ) ;if
      ) ;let*
    ) ;define

    (define (%run-conversion in-port out-port indent compact sort-keys
              ensure-ascii json-lines validate
            ) ;%run-conversion
      (if json-lines
        (let loop
          ((line-num 1) (has-error? #f))
          (let ((line (read-line in-port)))
            (cond ((eof-object? line) (if has-error? 1 0))
                  ((string-null? (string-trim-both line)) (loop (+ line-num 1) has-error?))
                  (else
                    (let ((res (catch #t (lambda () (string->json line)) (lambda (tag err) (cons tag err)))
                          ) ;res
                         ) ;
                      (if (and (pair? res) (symbol? (car res)))
                        (begin
                          (display (format #f "gf json: error on line ~a: ~a\n" line-num (cdr res))
                            (current-error-port)
                          ) ;display
                          (loop (+ line-num 1) #t)
                        ) ;begin
                        (begin
                          (unless validate
                            (write-json res
                              out-port
                              :indent       (if compact #f indent)
                              :sort-keys    sort-keys
                              :ensure-ascii ensure-ascii
                            ) ;write-json
                            (newline out-port)
                          ) ;unless
                          (loop (+ line-num 1) has-error?)
                        ) ;begin
                      ) ;if
                    ) ;let
                  ) ;else
            ) ;cond
          ) ;let
        ) ;let
        (let ((content (%read-all-port in-port)))
          (if (or (not content) (string-null? (string-trim-both content)))
            (begin
              (display "gf json: error: input is empty\n" (current-error-port))
              1
            ) ;begin
            (let ((res (catch #t (lambda () (string->json content)) (lambda (tag err) (cons tag err)))
                  ) ;res
                 ) ;
              (if (and (pair? res) (symbol? (car res)))
                (begin
                  (display (format #f "gf json: error: ~a\n" (cdr res)) (current-error-port))
                  1
                ) ;begin
                (begin
                  (unless validate
                    (write-json res
                      out-port
                      :indent       (if compact #f indent)
                      :sort-keys    sort-keys
                      :ensure-ascii ensure-ascii
                    ) ;write-json
                    (newline out-port)
                  ) ;unless
                  0
                ) ;begin
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (main)
      (let* ((raw-args (command-line))
             (idx (list-index (lambda (x) (string=? x "json")) raw-args))
             (args (if idx (drop raw-args (+ idx 1)) (if (pair? raw-args) (cdr raw-args) '()))
             ) ;args
            ) ;
        (run-goldjson args)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
