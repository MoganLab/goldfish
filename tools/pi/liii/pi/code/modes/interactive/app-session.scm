;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; /session 交互：向 worker 请求统计快照，并将结果作为信息卡片上屏
(define-library (liii pi code modes interactive app-session)
  (export pi-app-session-command?
    pi-app-submit-session!
    pi-app-handle-session-event!
  ) ;export
  (import (scheme base)
    (liii go)
    (liii string)
    (liii tui)
    (liii pi code core session-stats)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive views)
  ) ;import
  (begin

    (define (pi-app-session-command? text)
      (string=? text "/session")
    ) ;define

    (define (pi-app-submit-session! app)
      (editor-set-text! (pi-app-editor app) "")
      (catch #t
        (lambda () (chan-send! (pi-app-jobs-chan app) '(session_stats) 1000))
        (lambda (tag . args)
          (container-add! (%pi-app-chat app)
            (make-tool-card "session" '() 'error "Failed to read session stats")
          ) ;container-add!
        ) ;lambda
      ) ;catch
      (pi-app-render! app)
    ) ;define

    (define (pi-app-handle-session-event! app ev)
      (case (car ev)
            ((session_stats)
             (container-add! (%pi-app-chat app)
               (make-assistant-card (pi-format-session-stats (cadr ev)))
             ) ;container-add!
            ) ;
            ((session_stats_error)
             (container-add! (%pi-app-chat app)
               (make-tool-card "session" '() 'error (cadr ev))
             ) ;container-add!
            ) ;
            (else #f)
      ) ;case
    ) ;define

  ) ;begin
) ;define-library
