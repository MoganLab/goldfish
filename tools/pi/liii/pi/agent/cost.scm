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

(define-library (liii pi agent cost)
  (import (scheme base)
    (liii list)
    (liii pi cost)
    (liii pi message)
    (liii pi agent tools)
  ) ;import
  (export pi-agent-attach-cost pi-agent-session-cost-summary)
  (begin

    (define (pi-agent-attach-cost resp config)
      (if (not (or (pair? resp) (pi-assistant-message? resp)))
        resp
        (let* ((u (pi-agent-assoc-val 'usage "usage" resp #f))
               (provider (cdr (or (assoc 'provider config) '(#f . "google"))))
               (model-id (cdr (or (assoc 'model-id config) '(#f . ""))))
               (models-store (cdr (or (assoc 'models-store config) '(#f
                                                                     #_quote
                                                                     ()))))
              ) ;
          (if (not (pair? u))
            resp
            (let* ((cost-rates (pi-model-cost provider model-id models-store))
                   (cost (pi-calculate-cost cost-rates u))
                   (new-u
                     (append
                       (remove
                         (lambda (x) (or (eq? (car x) 'cost) (equal? (car x) "cost")))
                         u
                       ) ;remove
                       (list (cons 'cost cost))
                     ) ;append
                   ) ;new-u
                  ) ;
              (if (pi-assistant-message? resp)
                (pi-assistant-message (pi-assistant-message-content resp)
                  :stop-reason   (pi-assistant-message-stop-reason resp)
                  :error-message (pi-assistant-message-error-message resp)
                  :usage         new-u
                  :timestamp     (pi-assistant-message-timestamp resp)
                ) ;pi-assistant-message
                (append
                  (remove
                    (lambda (x) (or (eq? (car x) 'usage) (equal? (car x) "usage")))
                    resp
                  ) ;remove
                  (list (cons 'usage new-u))
                ) ;append
              ) ;if
            ) ;let*
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (pi-agent-session-cost-summary msgs config)
      (let* ((provider (cdr (or (assoc 'provider config) '(#f . "google"))))
             (model-id (cdr (or (assoc 'model-id config) '(#f . ""))))
             (models-store (cdr (or (assoc 'models-store config) '(#f
                                                                   #_quote
                                                                   ()))))
             (cost-rates (pi-model-cost provider model-id models-store))
             (usages '())
            ) ;
        (for-each
          (lambda (m)
            (when (equal? (pi-agent-assoc-val 'role "role" m "") "assistant")
              (let ((u (pi-agent-assoc-val 'usage "usage" m #f)))
                (when (pair? u)
                  (set! usages (cons u usages))
                ) ;when
              ) ;let
            ) ;when
          ) ;lambda
          msgs
        ) ;for-each
        (pi-summarize-session-cost cost-rates (reverse usages))
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
