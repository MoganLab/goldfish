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

(define-library (liii pi time)
  (import (scheme base) (liii string) (liii time))
  (export pi-iso-8601-utc-now pi-current-time-millis)
  (begin

    (define (%pad-two-digits n)
      (if (< n 10) (string-append "0" (number->string n)) (number->string n))
    ) ;define

    (define (%pad-three-digits n)
      (cond ((< n 10) (string-append "00" (number->string n)))
            ((< n 100) (string-append "0" (number->string n)))
            (else (number->string n))
      ) ;cond
    ) ;define

    (define (pi-iso-8601-utc-now)
      (let* ((now (current-time))
             (d (time-utc->date now 0))
             (year (number->string (date-year d)))
             (month (%pad-two-digits (date-month d)))
             (day (%pad-two-digits (date-day d)))
             (hour (%pad-two-digits (date-hour d)))
             (minute (%pad-two-digits (date-minute d)))
             (second (%pad-two-digits (date-second d)))
             (millis (%pad-three-digits (quotient (time-nanosecond now) 1000000)))
            ) ;
        (string-append year "-" month "-" day "T" hour ":" minute ":" second "."
          millis "Z"
        ) ;string-append
      ) ;let*
    ) ;define

    (define (pi-current-time-millis)
      (let ((now (current-time)))
        (+ (* (time-second now) 1000) (quotient (time-nanosecond now) 1000000))
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
