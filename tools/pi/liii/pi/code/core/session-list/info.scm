;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core session-list info)
  (import (scheme base) (liii base))
  (export make-pi-session-info pi-session-info? pi-session-info-path
    pi-session-info-id pi-session-info-cwd pi-session-info-name
    pi-session-info-created pi-session-info-modified
    pi-session-info-message-count pi-session-info-first-message
    pi-session-info-all-messages-text
  ) ;export
  (begin

    (define*-record-type <pi-session-info>
      (make-pi-session-info path
        id
        (cwd "")
        (name #f)
        (created "")
        (modified "")
        (message-count 0)
        (first-message "(no messages)")
        (all-messages-text "")
      ) ;make-pi-session-info
      pi-session-info?
      (path pi-session-info-path)
      (id pi-session-info-id)
      (cwd pi-session-info-cwd)
      (name pi-session-info-name)
      (created pi-session-info-created)
      (modified pi-session-info-modified)
      (message-count pi-session-info-message-count)
      (first-message pi-session-info-first-message)
      (all-messages-text %pi-session-info-all-messages-text-raw
        %pi-session-info-all-messages-text-set!
      ) ;all-messages-text
    ) ;define*-record-type

    (define (pi-session-info-all-messages-text info)
      (let ((val (%pi-session-info-all-messages-text-raw info)))
        (if (procedure? val)
          (let ((computed (val)))
            (%pi-session-info-all-messages-text-set! info computed)
            computed
          ) ;let
          val
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
