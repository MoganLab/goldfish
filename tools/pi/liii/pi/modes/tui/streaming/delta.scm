;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui streaming delta)
  (import (scheme base)
    (liii error)
    (liii pi event types)
    (liii pi modes tui message)
  ) ;import
  (export extract-stream-delta append-stream-chunk)
  (begin

    (define (%find-val alist key-sym key-str)
      (if (not (pair? alist))
        #f
        (let ((cell (or (assoc key-sym alist) (assoc key-str alist))))
          (if (pair? cell) (cdr cell) #f)
        ) ;let
      ) ;if
    ) ;define

    (define (%extract-from-pair ev)
      (let ((delta (%find-val ev 'delta "delta"))
            (text (%find-val ev 'text "text"))
            (partial (%find-val ev 'partialResult "partialResult"))
            (inner (%find-val ev 'assistantMessageEvent "assistantMessageEvent"))
           ) ;
        (cond ((string? delta) delta)
              ((string? text) text)
              ((string? partial) partial)
              ((or (pair? inner) (string? inner)) (extract-stream-delta inner))
              (else "")
        ) ;cond
      ) ;let
    ) ;define

    (define (extract-stream-delta ev)
      (cond ((string? ev) ev)
            ((pi-event-message-update? ev)
             (extract-stream-delta (pi-event-message-update-assistant-message-event ev))
            ) ;
            ((pi-event-tool-execution-update? ev)
             (let ((res (pi-event-tool-execution-update-partial-result ev)))
               (if (string? res) res "")
             ) ;let
            ) ;
            ((not (pair? ev)) "")
            (else (%extract-from-pair ev))
      ) ;cond
    ) ;define

    (define (%update-last-assistant-msg rev delta)
      (let* ((last-raw (car rev))
             (last-msg (normalize-pi-tui-message last-raw))
             (rest (cdr rev))
            ) ;
        (if (eq? (message-role last-msg) 'assistant)
          (let ((updated (make-pi-tui-message 'assistant
                           (string-append (message-content last-msg) delta)
                         ) ;make-pi-tui-message
                ) ;updated
               ) ;
            (reverse (cons updated rest))
          ) ;let
          (reverse (cons (make-pi-tui-message 'assistant delta) rev))
        ) ;if
      ) ;let*
    ) ;define

    (define (append-stream-chunk messages chunk)
      (unless (list? messages)
        (type-error "append-stream-chunk: expected list for messages")
      ) ;unless
      (let ((delta (if (string? chunk) chunk (extract-stream-delta chunk))))
        (if (null? messages)
          (list (make-pi-tui-message 'assistant delta))
          (%update-last-assistant-msg (reverse messages) delta)
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
