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
  (import (scheme base) (liii string) (liii date-time))
  (export pi-iso-8601-utc-now pi-current-time-millis pi-current-time-seconds)
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

    ;; 当前 UTC 的 POSIX 时间（精确有理数，含小数秒）
    (define (%current-utc-posix-time)
      (timestamp->utc-posix-time (current-utc-timestamp))
    ) ;define

    (define (pi-iso-8601-utc-now)
      (let* ((ts (current-utc-timestamp))
             (posix (timestamp->utc-posix-time ts))
             (year (number->string (timestamp-year ts)))
             (month (%pad-two-digits (timestamp-month ts)))
             (day (%pad-two-digits (timestamp-day ts)))
             (hour (%pad-two-digits (timestamp-hour ts)))
             (minute (%pad-two-digits (timestamp-minute ts)))
             (second (%pad-two-digits (floor (timestamp-second ts))))
             (millis
               (%pad-three-digits
                 (floor (* 1000 (- posix (floor posix))))
               ) ;%pad-three-digits
             ) ;millis
            ) ;
        (string-append year "-" month "-" day "T" hour ":" minute ":" second "."
          millis "Z"
        ) ;string-append
      ) ;let*
    ) ;define

    (define (pi-current-time-millis)
      (floor (* 1000 (%current-utc-posix-time)))
    ) ;define

    (define (pi-current-time-seconds)
      (floor (%current-utc-posix-time))
    ) ;define

  ) ;begin
) ;define-library
