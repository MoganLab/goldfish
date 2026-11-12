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

;; (hint scheme time) - (scheme time) 的类型接口

(define-library (hint scheme time)
  (import (scheme base))
  (export hint-scheme/time)
  (begin

    ;; 当前时间（秒，精确或非精确实数）
    (define hint-current-second '((-> number?)))

    ;; 滴答计数
    (define hint-current-jiffy '((-> integer?)))
    (define hint-jiffies-per-second '((-> integer?)))

    ;; 墙上时间：返回秒与微秒两个值
    (define hint-get-time-of-day '((-> any?)))

    ;; 单调时钟与时钟分辨率（纳秒）
    (define hint-monotonic-nanosecond '((-> integer?)))
    (define hint-system-clock-resolution '((-> integer?)))
    (define hint-steady-clock-resolution '((-> integer?)))

    (define (hint-scheme/time name)
      (case name
       ((current-second) hint-current-second)
       ((current-jiffy) hint-current-jiffy)
       ((jiffies-per-second) hint-jiffies-per-second)
       ((get-time-of-day) hint-get-time-of-day)
       ((monotonic-nanosecond) hint-monotonic-nanosecond)
       ((system-clock-resolution) hint-system-clock-resolution)
       ((steady-clock-resolution) hint-steady-clock-resolution)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
