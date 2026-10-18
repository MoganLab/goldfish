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

(define-library (liii pi retry)
  (import (scheme base) (liii pi config types) (liii pi event))
  (export pi-is-retriable-error? pi-calc-retry-delay pi-resolve-retry-params
    gemini-is-retriable-error? anthropic-is-retriable-error?
  ) ;export
  (begin

    (define (pi-is-retriable-error? status-code response-body-str)
      (cond ((= status-code 200) #f)
            ((= status-code 400) #f)
            ((= status-code 408) #t)
            ((= status-code 409) #t)
            ((= status-code 429) #t)
            ((>= status-code 500) #t)
            ((= status-code 0) #t)
            (else #f)
      ) ;cond
    ) ;define

    (define gemini-is-retriable-error? pi-is-retriable-error?)
    (define anthropic-is-retriable-error? pi-is-retriable-error?)

    (define (pi-calc-retry-delay attempt base-delay-ms max-agent-delay-ms)
      (let ((delay-ms
              (min
                (* base-delay-ms (expt 2 (max 0 (- attempt 1))))
                max-agent-delay-ms
              ) ;min
            ) ;delay-ms
           ) ;
        (max 0.001 (/ delay-ms 1000.0))
      ) ;let
    ) ;define

    (define (%safe-assoc key alist)
      (and (or (pair? alist) (pi-settings? alist) (pi-retry-config? alist))
        (or (assoc key alist)
          (let ((str-k (if (symbol? key) (symbol->string key) (string->symbol key))))
            (assoc str-k alist)
          ) ;let
        ) ;or
      ) ;and
    ) ;define

    (define (pi-resolve-retry-params settings opt-retry)
      (let* ((retry-cfg
               (or opt-retry (cdr (or (%safe-assoc 'retry settings) '(#f))))
             ) ;retry-cfg
             (enabled? (cdr (or (%safe-assoc 'enabled retry-cfg) '(#f . #t))))
             (max-retries
               (if enabled? (cdr (or (%safe-assoc 'max-retries retry-cfg) '(#f
                                                                            . 3))) 0)
             ) ;max-retries
             (base-delay (cdr (or (%safe-assoc 'base-delay-ms retry-cfg) '(#f
                                                                           . 2000))))
             (max-delay (cdr (or (%safe-assoc 'max-agent-delay-ms retry-cfg) '(#f
                                                                               . 60000))))
            ) ;
        (values max-retries base-delay max-delay)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
