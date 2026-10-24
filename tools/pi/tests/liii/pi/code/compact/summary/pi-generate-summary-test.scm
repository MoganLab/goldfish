(import (liii check)
        (liii string)
        (liii pi ai message) (liii pi code compact summary)
        (liii pi code compact))

(check-set-mode! 'report-failed)

;; pi-summarization-failure: error / length 均视为失败
(let ((err-msg (pi-assistant-message (list (pi-text-block ""))
                 :stop-reason "error"
                 :error-message "boom"
               ) ;pi-assistant-message
      ) ;err-msg
     ) ;
  (check-true (string-contains? (pi-summarization-failure err-msg "Summarization")
                "boom"
              ) ;string-contains?
  ) ;check-true
) ;let

(let ((length-msg (pi-assistant-message (list (pi-text-block "partial"))
                    :stop-reason "length"
                  ) ;pi-assistant-message
      ) ;length-msg
     ) ;
  (check-true (string-contains? (pi-summarization-failure length-msg "Summarization")
                "token cap"
              ) ;string-contains?
  ) ;check-true
) ;let

(check (pi-summarization-failure
         (pi-assistant-message (list (pi-text-block "ok")))
         "Summarization"
       ) ;pi-summarization-failure
  => #f
) ;check

(check-report)
