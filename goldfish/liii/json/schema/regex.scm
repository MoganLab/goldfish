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

(define-library (liii json schema regex)
  (import (scheme base) (liii subprocess) (liii string))
  (export regex-search? regex-valid?)
  (begin
    ;; First backend: PCRE through grep -P. Arguments and input never use a shell.
    (define (normalize-pattern pattern)
      (string-append "(*UTF)"
        (string-replace (string-replace pattern "\\p{Letter}" "\\p{L}")
          "\\p{Number}"
          "\\p{N}"
        ) ;string-replace
      ) ;string-append
    ) ;define
    ;; Returns #t when the pattern compiles with the backend.
    (define (regex-valid? pattern)
      (unless (string? pattern)
        (error 'json-schema-schema-error "Regex pattern must be a string")
      ) ;unless
      (let ((code
              (guard (condition (#t
                                  (error 'json-schema-options-error "PCRE regex backend is unavailable" condition)
                                ) ;
                     ) ;condition
                (run (list "grep" "-P" "-q" "--" (normalize-pattern pattern))
                  :input   ""
                  :stdout  'discard
                  :stderr  'discard
                  :timeout 5
                ) ;run
              ) ;guard
            ) ;code
           ) ;
        ;; 0 means matched, 1 means valid but no match, 2 means invalid.
        (<= code 1)
      ) ;let
    ) ;define

    (define (regex-search? pattern value)
      (unless (and (string? pattern) (string? value))
        (error 'json-schema-schema-error "Regex pattern and input must be strings")
      ) ;unless
      (let ((code
              (guard (condition (#t
                                  (error 'json-schema-options-error "PCRE regex backend is unavailable" condition)
                                ) ;
                     ) ;condition
                (run (list "grep" "-P" "-q" "--" (normalize-pattern pattern))
                  :input   (string-append value "\n")
                  :stdout  'discard
                  :stderr  'discard
                  :timeout 5
                ) ;run
              ) ;guard
            ) ;code
           ) ;
        (cond ((= code 0) #t)
              ((= code 1) #f)
              (else (error 'json-schema-schema-error "Invalid or unsupported regex" pattern))
        ) ;cond
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
