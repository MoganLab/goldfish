;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi agent cost)
  (import (scheme base)
    (liii list)
    (liii pi ai config runtime)
    (liii pi ai cost)
    (liii pi ai message)
    (liii pi agent tools)
  ) ;import
  (export pi-agent-attach-cost pi-agent-session-cost-summary)
  (begin

    (define (pi-agent-attach-cost resp config)
      (if (not (or (pair? resp) (pi-assistant-message? resp)))
        resp
        (let* ((u (if (pi-assistant-message? resp)
                    (pi-assistant-message-usage resp)
                    (pi-agent-assoc-val 'usage "usage" resp #f)
                  ) ;if
               ) ;u
               (provider (pi-cfg-ref config 'provider "google"))
               (model-id (pi-cfg-ref config 'model-id ""))
               (models-store (pi-cfg-ref config 'models-store '()))
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
                  :provider      (pi-assistant-message-provider resp)
                  :model         (pi-assistant-message-model resp)
                  :api           (pi-assistant-message-api resp)
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
      (let* ((provider (pi-cfg-ref config 'provider "google"))
             (model-id (pi-cfg-ref config 'model-id ""))
             (models-store (pi-cfg-ref config 'models-store '()))
             (cost-rates (pi-model-cost provider model-id models-store))
            ) ;
        (pi-summarize-session-cost cost-rates
          (filter pair?
            (map
              (lambda (raw)
                (let ((m (or (json->pi-message raw) raw)))
                  (and (pi-assistant-message? m) (pi-assistant-message-usage m))
                ) ;let
              ) ;lambda
              msgs
            ) ;map
          ) ;filter
        ) ;pi-summarize-session-cost
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
