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

(define-library (liii packrat result)
  (import (scheme base) (liii packrat position) (liii packrat error))
  (export
    ;; parse-result
    parse-result?
    parse-result-successful?
    parse-result-semantic-value
    parse-result-next
    parse-result-error
    make-result
    make-expected-result
    make-message-result
    merge-result-errors
    parse-error->parse-result

    ;; parse-results
    parse-results?
    parse-results-position
    parse-results-next
    parse-results-token-kind
    parse-results-token-value
    parse-results-base
    base-generator->results
    prepend-base
    prepend-semantic-value
    results->result
  ) ;export
  (begin
    (define-record-type <parse-result>
      (make-parse-result successful? semantic-value next error)
      parse-result?
      (successful? parse-result-successful?)
      (semantic-value parse-result-semantic-value)
      (next parse-result-next)
      (error parse-result-error)
    ) ;define-record-type

    (define-record-type <parse-results>
      (make-parse-results position base next map)
      parse-results?
      (position parse-results-position)
      (base parse-results-base)
      (next parse-results-next* set-parse-results-next!)
      (map parse-results-map set-parse-results-map!)
    ) ;define-record-type

    (define (empty-results pos)
      (make-parse-results pos #f #f '())
    ) ;define

    (define (make-results pos base next-generator)
      (make-parse-results pos base next-generator '())
    ) ;define

    (define (make-result semantic-value next)
      (make-parse-result #t semantic-value next #f)
    ) ;define

    (define (parse-error->parse-result err)
      (make-parse-result #f #f #f err)
    ) ;define

    (define (make-expected-result pos str)
      (parse-error->parse-result (make-error-expected pos str))
    ) ;define

    (define (make-message-result pos msg)
      (parse-error->parse-result (make-error-message pos msg))
    ) ;define

    (define (prepend-base pos base next)
      (make-parse-results pos base next '())
    ) ;define

    (define (prepend-semantic-value pos key result next)
      (make-parse-results pos #f #f (list (cons key (make-result result next))))
    ) ;define

    (define (base-generator->results generator)
      (define (results-generator)
        (let-values (((pos base) (generator)))
          (if (not base) (empty-results pos) (make-results pos base results-generator))
        ) ;let-values
      ) ;define
      (results-generator)
    ) ;define

    (define (parse-results-next results)
      (let ((next (parse-results-next* results)))
        (if (procedure? next)
          (let ((next-value (next)))
            (set-parse-results-next! results next-value)
            next-value
          ) ;let
          next
        ) ;if
      ) ;let
    ) ;define

    (define (results->result results key fn)
      (let ((results-map (parse-results-map results)))
        (cond
         ((assv key results-map)
          =>
          (lambda (entry)
            (if (not (cdr entry)) (error "Recursive parse rule" key) (cdr entry))
          ) ;lambda
         ) ;
         (else
           (let ((cell (cons key #f)))
             (set-parse-results-map! results (cons cell results-map))
             (let ((result (fn)))
               (set-cdr! cell result)
               result
             ) ;let
           ) ;let
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (merge-result-errors result errs)
      (make-parse-result (parse-result-successful? result)
        (parse-result-semantic-value result)
        (parse-result-next result)
        (merge-parse-errors (parse-result-error result) errs)
      ) ;make-parse-result
    ) ;define

    (define (parse-results-token-kind results)
      (let ((base (parse-results-base results)))
        (and base (car base))
      ) ;let
    ) ;define

    (define (parse-results-token-value results)
      (let ((base (parse-results-base results)))
        (and base (cdr base))
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
