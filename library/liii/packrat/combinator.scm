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

(define-library (liii packrat combinator)
  (import (scheme base) (liii packrat result))
  (export packrat-check-base packrat-check packrat-or packrat-unless)
  (begin
    (define (packrat-check-base token-kind k)
      (lambda (results)
        (let ((base (parse-results-base results)))
          (if (eqv? (and base (car base)) token-kind)
           ((k (and base (cdr base))) (parse-results-next results))
           (make-expected-result (parse-results-position results)
             (if (not token-kind) "end-of-file" token-kind)
           ) ;make-expected-result
          ) ;if
        ) ;let
      ) ;lambda
    ) ;define

    (define (packrat-check parser k)
      (lambda (results)
        (let ((result (parser results)))
          (if (parse-result-successful? result)
            (merge-result-errors ((k (parse-result-semantic-value result)) (parse-result-next result))
              (parse-result-error result)
            ) ;merge-result-errors
            result
          ) ;if
        ) ;let
      ) ;lambda
    ) ;define

    (define (packrat-or p1 p2)
      (lambda (results)
        (let ((result (p1 results)))
          (if (parse-result-successful? result)
            result
            (merge-result-errors (p2 results) (parse-result-error result))
          ) ;if
        ) ;let
      ) ;lambda
    ) ;define

    (define (packrat-unless explanation p1 p2)
      (lambda (results)
        (let ((result (p1 results)))
          (if (parse-result-successful? result)
            (make-message-result (parse-results-position results) explanation)
            (p2 results)
          ) ;if
        ) ;let
      ) ;lambda
    ) ;define
  ) ;begin
) ;define-library
