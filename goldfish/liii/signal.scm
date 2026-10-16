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
;;

(define-library (liii signal)
  (export signal-trap signal-get signal-raise)
  (import (scheme base) (liii base) (liii error) (liii hash-table))
  (begin
    (define *signal-table* (g_signal-supported-signals))

    (define *handlers* (make-hash-table))

    (for-each (lambda (entry) (hash-table-set! *handlers* (car entry) 'default))
      *signal-table*
    ) ;for-each

    (define (dispatch-signal sig)
      (let ((h (hash-table-ref/default *handlers* sig 'default)))
        (if (procedure? h) (h sig))
      ) ;let
    ) ;define

    (g_signal-set-dispatcher dispatch-signal)

    (define (signal-trap sig handler)
      (if (not (symbol? sig))
        (type-error "(signal-trap sig handler): sig must be a symbol" sig)
      ) ;if
      (let ((entry (assq sig *signal-table*)))
        (if (not entry)
          (value-error "(signal-trap sig handler): unsupported signal" sig)
        ) ;if
        (if (not (or (procedure? handler) (eq? handler 'default) (eq? handler 'ignore)))
          (type-error "(signal-trap sig handler): handler must be a procedure, 'default, or 'ignore"
            handler
          ) ;type-error
        ) ;if
        (let ((old-handler (hash-table-ref/default *handlers* sig 'default))
              (signum (cdr entry))
              (mode (cond ((eq? handler 'default) 'default)
                          ((eq? handler 'ignore) 'ignore)
                          (else 'catch)
                    ) ;cond
              ) ;mode
             ) ;
          (g_signal-os-trap signum mode)
          (hash-table-set! *handlers* sig handler)
          old-handler
        ) ;let
      ) ;let
    ) ;define

    (define (signal-get sig)
      (if (not (symbol? sig))
        (type-error "(signal-get sig): sig must be a symbol" sig)
      ) ;if
      (let ((entry (assq sig *signal-table*)))
        (if (not entry) (value-error "(signal-get sig): unsupported signal" sig))
        (hash-table-ref/default *handlers* sig 'default)
      ) ;let
    ) ;define

    (define (signal-raise sig)
      (if (not (symbol? sig))
        (type-error "(signal-raise sig): sig must be a symbol" sig)
      ) ;if
      (let ((entry (assq sig *signal-table*)))
        (if (not entry) (value-error "(signal-raise sig): unsupported signal" sig))
        (g_signal-raise (cdr entry))
        (values)
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
