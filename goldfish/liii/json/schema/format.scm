;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;  http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii json schema format)
  (import (scheme base)
    (scheme char)
    (liii json schema data)
    (liii json schema error)
    (liii json schema model)
    (liii json schema format-builtin)
    (liii string)
  ) ;import
  (export validate-format json-schema-format-checker? format-checker-applies?
    json-schema-builtin-format-checkers
  ) ;export
  (begin
    ;; A format checker is a procedure from a string to a boolean.
    ;; Returning #f means the value does not match the format.
    (define (json-schema-format-checker? value)
      (procedure? value)
    ) ;define
    ;; Built-in checkers are looked up by their lower-case format name.
    ;; The registry is filled in by the format specific tasks.
    (define builtin-checkers json-schema-format-builtins)
    (define (json-schema-builtin-format-checkers)
      builtin-checkers
    ) ;define
    (define (format-name->symbol format)
      (if (symbol? format) format (string->symbol (string-downcase format)))
    ) ;define
    (define (lookup-checker format custom)
      (let ((name (format-name->symbol format)))
        (let ((cell (assoc name custom)))
          (if cell
            (cdr cell)
            (let ((builtin (assoc name builtin-checkers)))
              (if builtin (cdr builtin) #f)
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define
    ;; The formats option selects which formats are asserted.
    ;;   #f      -- no format is asserted, format is an annotation only
    ;;   #t      -- every format with a known checker is asserted
    ;;   (list)  -- only the listed formats are asserted
    (define (format-checker-applies? format options)
      (let ((selected (option-ref options 'formats #f)))
        (cond ((not selected) #f)
              ((eq? selected #t) #t)
              (else (and (proper-list? selected) (member (format-name->symbol format) selected))
              ) ;else
        ) ;cond
      ) ;let
    ) ;define
    (define (format-checker-for format options)
      (lookup-checker format (option-ref options 'format-checkers '()))
    ) ;define
    (define (format-error format instance schema path schema-path)
      (list (validation-error "Value does not match format"
              "format"
              format
              instance
              schema
              path
              schema-path
              '()
              #f
            ) ;validation-error
      ) ;list
    ) ;define
    (define (validate-format validator format instance schema path schema-path)
      (let ((options (validator-options validator)))
        (if (not (string? instance))
          '()
          (if (not (format-checker-applies? format options))
            '()
            (let ((checker (format-checker-for format options)))
              (if (not checker)
                '()
                (if (not (json-schema-format-checker? checker))
                  (error 'json-schema-options-error "Format checker must be a procedure" checker)
                  (if (checker instance)
                    '()
                    (format-error format instance schema path schema-path)
                  ) ;if
                ) ;if
              ) ;if
            ) ;let
          ) ;if
        ) ;if
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
