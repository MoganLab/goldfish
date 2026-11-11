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

(define-library (liii packrat position)
  (import (scheme base))
  (export make-parse-position parse-position? parse-position-file
    parse-position-line parse-position-column top-parse-position
    update-parse-position parse-position->string parse-position>?
  ) ;export
  (begin
    (define-record-type <parse-position>
      (make-parse-position file line column)
      parse-position?
      (file parse-position-file)
      (line parse-position-line)
      (column parse-position-column)
    ) ;define-record-type

    (define (top-parse-position filename)
      (make-parse-position filename 1 0)
    ) ;define

    (define (update-parse-position pos ch)
      (if (not pos)
        #f
        (let ((file (parse-position-file pos))
              (line (parse-position-line pos))
              (column (parse-position-column pos))
             ) ;
          (case ch
           ((#\return) (make-parse-position file line 0))
           ((#\newline) (make-parse-position file (+ line 1) 0))
           ((#\tab) (make-parse-position file line (* (quotient (+ column 8) 8) 8)))
           (else (make-parse-position file line (+ column 1)))
          ) ;case
        ) ;let
      ) ;if
    ) ;define

    (define (parse-position->string pos)
      (if (not pos)
        "<??>"
        (string-append (parse-position-file pos)
          ":"
          (number->string (parse-position-line pos))
          ":"
          (number->string (parse-position-column pos))
        ) ;string-append
      ) ;if
    ) ;define

    (define (parse-position>? a b)
      (cond ((not a) #f)
            ((not b) #t)
            (else
              (let ((la (parse-position-line a)) (lb (parse-position-line b)))
                (or (> la lb)
                  (and (= la lb) (> (parse-position-column a) (parse-position-column b)))
                ) ;or
              ) ;let
            ) ;else
      ) ;cond
    ) ;define
  ) ;begin
) ;define-library
