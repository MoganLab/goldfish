;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi tui handler)
  (export make-tui-agent-handler)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii list)
    (liii pi message)
    (liii pi agent queue)
    (liii pi tui app)
    (liii pi modes flow)
    (liii pi modes output)
  ) ;import
  (begin

    (define (%extract-text-from-content content)
      (let* ((blks (cond ((vector? content) (vector->list content))
                         ((list? content) content)
                         (else '())
                   ) ;cond
             ) ;blks
             (texts
               (map
                 (lambda (b)
                   (let* ((alist (cond ((pi-block? b) (pi-block->alist b)) ((pair? b) b) (else '())))
                          (type (cdr (or (assoc 'type alist) (assoc "type" alist) '(#f
                                                                                    . ""))))
                          (txt (cdr (or (assoc 'text alist) (assoc "text" alist) '(#f
                                                                                   . ""))))
                         ) ;
                     (if (and (equal? type "text") (string? txt)) txt "")
                   ) ;let*
                 ) ;lambda
                 blks
               ) ;map
             ) ;texts
            ) ;
        (apply string-append texts)
      ) ;let*
    ) ;define

    (define (%handle-tui-tool-end app args)
      (let ((id (if (pair? args) (car args) ""))
            (name (if (> (length args) 1) (cadr args) "tool"))
            (raw-res (if (> (length args) 2) (caddr args) ""))
            (is-err? (if (> (length args) 3) (cadddr args) #f))
           ) ;
        (pi-app-add-tool-result! app name '() (if is-err? 'error 'success) raw-res)
      ) ;let
    ) ;define

    (define (%handle-tui-message-end app args)
      (when (pair? args)
        (let* ((msg (car args))
               (alist (cond ((pi-message? msg) (pi-message->alist msg))
                            ((pair? msg) msg)
                            (else '())
                      ) ;cond
               ) ;alist
               (role (cdr (or (assoc 'role alist) (assoc "role" alist) '(#f
                                                                         . ""))))
               (content (cdr (or (assoc 'content alist) (assoc "content" alist) '(#f
                                                                                  #_quote
                                                                                  ())))
               ) ;content
               (text (%extract-text-from-content content))
              ) ;
          (when
            (and (equal? role "assistant") (not (string-null? (string-trim-both text))))
            (pi-app-add-assistant-text! app text)
          ) ;when
        ) ;let*
      ) ;when
    ) ;define

    (define (%make-tui-emitter app)
      (lambda (event-type . args)
        (cond ((eq? event-type 'tool_execution_end) (%handle-tui-tool-end app args))
              ((eq? event-type 'message_end) (%handle-tui-message-end app args))
              ((eq? event-type 'agent_settled)
               (pi-queue-finish-task! (pi-app-queue app))
               (pi-app-drain-queue! app)
              ) ;
              (else #t)
        ) ;cond
      ) ;lambda
    ) ;define

    (define* (make-tui-agent-handler parsed cfg (runner run-session-flow))
      (let* ((provider (cdr (or (assoc 'provider cfg) '(#f . "google"))))
             (api-key (cdr (or (assoc 'api-key cfg) '(#f . #f))))
            ) ;
        (lambda (app user-text)
          (if (or (not api-key) (and (string? api-key) (string-null? api-key)))
            (begin
              (pi-app-add-tool-result! app
                "auth"
                '()
                'error
                (string-append "No credentials found for provider \"" provider "\"")
              ) ;pi-app-add-tool-result!
              (pi-queue-finish-task! (pi-app-queue app))
              (pi-app-drain-queue! app)
            ) ;begin
            (let ((tui-emit (%make-tui-emitter app)))
              (catch #t
                (lambda () (runner cfg (list (pi-user-message user-text)) #f tui-emit parsed))
                (lambda (tag . err-args)
                  (pi-app-add-tool-result! app
                    "error"
                    '()
                    'error
                    (string-append (object->string tag) ": " (object->string err-args))
                  ) ;pi-app-add-tool-result!
                ) ;lambda
              ) ;catch
            ) ;let
          ) ;if
        ) ;lambda
      ) ;let*
    ) ;define*
  ) ;begin

) ;define-library
