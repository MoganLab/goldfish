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

(define-library (liii pi simplify parse)
  (import (scheme base) (liii string) (liii list) (liii json))
  (export pi-parse-simplify-suggestions)
  (begin

    (define (%jref obj key default)
      (if (not (pair? obj))
        default
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (or (assoc str-key obj) (assoc sym-key obj)))
              ) ;
          (if pair (cdr pair) default)
        ) ;let*
      ) ;if
    ) ;define

    (define (%strip-fence text)
      (let ((trimmed (string-trim-both text)))
        (if (string-starts? trimmed "```")
          (let* ((first-newline (string-index trimmed #\newline))
                 (body
                   (if first-newline
                     (string-trim-both (substring trimmed (+ first-newline 1) (string-length trimmed))
                     ) ;string-trim-both
                     ""
                   ) ;if
                 ) ;body
                ) ;
            (if (string-ends? body "```")
              (string-trim-both (substring body 0 (- (string-length body) 3)))
              body
            ) ;if
          ) ;let*
          trimmed
        ) ;if
      ) ;let
    ) ;define

    (define (%extract-json-text text)
      (let* ((trimmed (string-trim-both text))
             (array? (string-starts? trimmed "["))
             (open (if array? #\[ #\{))
             (close (if array? #\] #\}))
             (start (string-index trimmed open))
             (end (and start (string-index-right trimmed close)))
            ) ;
        (if (and start end (> end start)) (substring trimmed start (+ end 1)) trimmed)
      ) ;let*
    ) ;define

    (define (%truncate-raw text)
      (if (<= (string-length text) 200) text (substring text 0 200))
    ) ;define

    (define (%fallback text)
      (list
        (list (cons 'file "")
          (cons 'category "parse-error")
          (cons 'message
            (string-append "Could not parse structured simplification suggestions. Raw response: "
              (%truncate-raw text)
            ) ;string-append
          ) ;cons
        ) ;list
      ) ;list
    ) ;define

    (define (%vector-or-list->list value)
      (cond ((vector? value) (vector->list value))
            ((list? value) value)
            (else '())
      ) ;cond
    ) ;define

    (define (%non-empty-string? value)
      (and (string? value) (not (string-null? (string-trim-both value))))
    ) ;define

    (define (%valid? item)
      (and (pair? item)
        (%non-empty-string? (%jref item "file" #f))
        (%non-empty-string? (%jref item "category" #f))
        (%non-empty-string? (%jref item "message" #f))
      ) ;and
    ) ;define

    (define (%normalize-line value)
      (cond ((number? value) value)
            ((string? value) (string->number value))
            (else #f)
      ) ;cond
    ) ;define

    (define (%normalize item)
      (let* ((line (%normalize-line (%jref item "line" #f)))
             (suggestion (%jref item "suggestion" #f))
             (base (list (cons 'file (%jref item "file" ""))
                     (cons 'category (%jref item "category" ""))
                     (cons 'message (%jref item "message" ""))
                   ) ;list
             ) ;base
            ) ;
        (append base
          (if line (list (cons 'line line)) '())
          (if (string? suggestion) (list (cons 'suggestion suggestion)) '())
        ) ;append
      ) ;let*
    ) ;define

    (define (%raw-suggestions parsed)
      (cond ((vector? parsed) parsed)
            ((and (list? parsed)
               (or (assoc "suggestions" parsed) (assoc 'suggestions parsed))
             ) ;and
             (%jref parsed "suggestions" #f)
            ) ;
            ((and (pair? parsed) (or (string? (caar parsed)) (symbol? (caar parsed)))) #f)
            ((list? parsed) parsed)
            (else #f)
      ) ;cond
    ) ;define

    (define (%extract-suggestions text)
      (let ((parsed
              (catch #t
                (lambda () (string->json (%extract-json-text (%strip-fence text))))
                (lambda _ #f)
              ) ;catch
            ) ;parsed
           ) ;
        (if (not (or (pair? parsed) (vector? parsed)))
          #f
          (let ((raw (%raw-suggestions parsed)))
            (if raw
              (filter (lambda (item) item)
                (map (lambda (item) (and (%valid? item) (%normalize item)))
                  (%vector-or-list->list raw)
                ) ;map
              ) ;filter
              #f
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (pi-parse-simplify-suggestions text)
      (let ((suggestions (%extract-suggestions (or text ""))))
        (if suggestions suggestions (%fallback (or text "")))
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
