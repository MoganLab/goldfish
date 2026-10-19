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

(define-library (liii pi modes)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii pi args)
    (liii pi config)
    (liii pi modes output)
    (liii pi modes flow)
    (liii pi modes review)
  ) ;import
  (export display-stderr handle-print-result emit-json-event dispatch-pi
    prepare-system-prompt persist-initial-user-msg! run-agent-loop-with-timeout
    run-session-flow run-compact-flow run-code-review-flow
    pi-auto-review-system-prompt
  ) ;export
  (begin

    (define (dispatch-pi parsed mode initial-msg rest-msgs)
      (let* ((env (get-environment-variables))
             (cfg (resolve-pi-config parsed env))
             (provider (cdr (or (assoc 'provider cfg) '(#f . "google"))))
             (api-key (cdr (or (assoc 'api-key cfg) '(#f . #f))))
            ) ;
        (when (or (not api-key) (and (string? api-key) (string-null? api-key)))
          (error 'credential-error
            (string-append "No credentials found for provider \"" provider "\"")
          ) ;error
        ) ;when
        (cond
         ((pi-args-ref parsed 'compact)
          (run-compact-flow parsed
            cfg
            initial-msg
            (if (eq? mode 'json) emit-json-event #f)
          ) ;run-compact-flow
         ) ;
         ((pi-args-ref parsed 'code-review) (run-code-review-flow parsed cfg mode))
         ((eq? mode 'print)
          (run-session-flow parsed
            cfg
            initial-msg
            #f
            (lambda (res-msgs)
              (handle-print-result (and (pair? res-msgs) (car (reverse res-msgs))))
            ) ;lambda
          ) ;run-session-flow
         ) ;
         ((eq? mode 'json)
          (run-session-flow parsed cfg initial-msg emit-json-event (lambda _ 0))
         ) ;
         (else 0)
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
