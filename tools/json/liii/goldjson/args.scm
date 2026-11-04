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

(define-library (liii goldjson args)
  (import (scheme base) (scheme write) (liii string))
  (export parse-json-args display-json-help)
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

  ) ;begin
) ;define-library
