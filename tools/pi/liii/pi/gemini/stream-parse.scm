;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi gemini stream-parse)
  (import (scheme base)
    (liii string)
    (liii json)
    (liii sse)
    (liii pi message)
    (liii pi gemini core)
    (liii pi gemini thinking)
  ) ;import
  (export emit-text-start-and-delta append-text-block
    process-stream-function-call process-stream-part process-sse-chunk-data
    make-stream-decoder extract-stream-error-message format-failed-stream-msg
  ) ;export
  (begin

    (define (%resp-field resp key)
      (cond ((procedure? resp) (resp key))
            ((hash-table? resp) (resp key))
            ((pair? resp) (cdr (or (assoc key resp) '(#f . #f))))
            (else #f)
      ) ;cond
    ) ;define

    (define (format-failed-stream-msg status resp)
      (let* ((resp-err (or (%resp-field resp 'reason) (%resp-field resp 'error) ""))
             (err-text (if (string? resp-err) (string-trim resp-err) ""))
            ) ;
        (if (not (string-null? err-text))
          (string-append "Request failed: " err-text)
          (string-append "HTTP " (number->string status) " stream error")
        ) ;if
      ) ;let*
    ) ;define

    (define (extract-stream-error-message body)
      (catch #t
        (lambda ()
          (let ((json-data (string->json body)))
            (and (pair? json-data)
              (let ((err (cdr (or (assoc "error" json-data) '(#f . #f)))))
                (cond
                 ((pair? err) (cdr (or (assoc "message" err) '(#f . #f))))
                 ((string? err) err)
                 (else #f)
                ) ;cond
              ) ;let
            ) ;and
          ) ;let
        ) ;lambda
        (lambda (tag . args) #f)
      ) ;catch
    ) ;define

    (define (emit-text-start-and-delta on-event cur-idx text usage)
      (when (procedure? on-event)
        (on-event (list (cons 'type "text_start")
                    (cons 'contentIndex cur-idx)
                    (cons 'usage usage)
                  ) ;list
        ) ;on-event
        (on-event (list (cons 'type "text_delta")
                    (cons 'contentIndex cur-idx)
                    (cons 'delta text)
                    (cons 'usage usage)
                  ) ;list
        ) ;on-event
      ) ;when
    ) ;define

    (define (append-text-block blocks last-text t cur-idx usage stream-state on-event)
      (if (string-null? t)
        #t
        (if (not (string? last-text))
          (begin
            (vector-set! stream-state 0 (append blocks (list (pi-text-block t))))
            (vector-set! stream-state 1 t)
            (emit-text-start-and-delta on-event cur-idx t usage)
          ) ;begin
          (let* ((new-t (string-append last-text t))
                 (upd-blocks
                   (let loop
                     ((lst blocks) (acc '()))
                     (if (null? (cdr lst))
                       (append (reverse acc) (list (pi-text-block new-t)))
                       (loop (cdr lst) (cons (car lst) acc))
                     ) ;if
                   ) ;let
                 ) ;upd-blocks
                ) ;
            (vector-set! stream-state 0 upd-blocks)
            (vector-set! stream-state 1 new-t)
            (when (procedure? on-event)
              (on-event (list (cons 'type "text_delta")
                          (cons 'contentIndex cur-idx)
                          (cons 'delta t)
                          (cons 'usage usage)
                        ) ;list
              ) ;on-event
            ) ;when
          ) ;let*
        ) ;if
      ) ;if
    ) ;define

    (define (process-stream-function-call fc blocks stream-state on-event)
      (let* ((name (cdr (assoc "name" fc)))
             (args (cdr (assoc "args" fc)))
             (id (cdr (or (assoc "id" fc) (assoc 'id fc) '(#f . #f))))
             (ts (cdr (or (assoc "thoughtSignature" fc) (assoc 'thoughtSignature fc) '(#f
                                                                                       . #f)))
             ) ;ts
             (eff-id
               (if (and (string? id) (not (string-null? id)))
                 id
                 (gemini-generate-tool-call-id name 0 (+ (length blocks) 1))
               ) ;if
             ) ;eff-id
             (tc-blk (pi-tool-call-block eff-id name args :thought-signature ts))
             (cur-idx (length blocks))
             (tool-calls (vector-ref stream-state 3))
             (usage (vector-ref stream-state 2))
            ) ;
        (vector-set! stream-state 0 (append blocks (list tc-blk)))
        (vector-set! stream-state 1 #f)
        (vector-set! stream-state 3 (append tool-calls (list tc-blk)))
        (when (procedure? on-event)
          (on-event (list (cons 'type "toolcall_start")
                      (cons 'contentIndex cur-idx)
                      (cons 'id eff-id)
                      (cons 'toolName name)
                      (cons 'usage usage)
                    ) ;list
          ) ;on-event
          (on-event (list (cons 'type "toolcall_delta")
                      (cons 'contentIndex cur-idx)
                      (cons 'delta (json->string args))
                      (cons 'usage usage)
                    ) ;list
          ) ;on-event
          (on-event (list (cons 'type "toolcall_end")
                      (cons 'contentIndex cur-idx)
                      (cons 'id eff-id)
                      (cons 'toolName name)
                      (cons 'usage usage)
                    ) ;list
          ) ;on-event
        ) ;when
      ) ;let*
    ) ;define

    (define (process-stream-part part stream-state on-event)
      (let* ((blocks (vector-ref stream-state 0))
             (last-text (vector-ref stream-state 1))
             (usage (vector-ref stream-state 2))
             (part-ts (cdr (or (assoc "thoughtSignature" part) (assoc 'thoughtSignature part) '(#f
                                                                                                . #f))
                      ) ;cdr
             ) ;part-ts
            ) ;
        (cond
         ((assoc "text" part)
          (let ((t (cdr (assoc "text" part))))
            (if (gemini-is-thinking-part? part)
              (gemini-append-thinking-block! stream-state t part-ts on-event)
              (begin
                (when (equal? (gemini-stream-state-active-type stream-state) "thinking")
                  (gemini-stream-stop-active-block! stream-state on-event)
                ) ;when
                (vector-set! stream-state 5 "text")
                (let* ((blocks (vector-ref stream-state 0))
                       (last-text (vector-ref stream-state 1))
                       (cur-idx (if (string? last-text) (- (length blocks) 1) (length blocks)))
                      ) ;
                  (append-text-block blocks last-text t cur-idx usage stream-state on-event)
                ) ;let*
              ) ;begin
            ) ;if
          ) ;let
         ) ;
         ((assoc "functionCall" part)
          (gemini-stream-stop-active-block! stream-state on-event)
          (let ((fc (cdr (assoc "functionCall" part))))
            (process-stream-function-call
              (if (and (string? part-ts) (not (assoc "thoughtSignature" fc)))
                (cons (cons "thoughtSignature" part-ts) fc)
                fc
              ) ;if
              blocks
              stream-state
              on-event
            ) ;process-stream-function-call
          ) ;let
         ) ;
        ) ;cond
      ) ;let*
    ) ;define

    (define (process-sse-chunk-data json-obj stream-state on-event)
      (let* ((candidates (cdr (or (assoc "candidates" json-obj) '(#f . #()))))
             (usage-meta (cdr (or (assoc "usageMetadata" json-obj) '(#f . #f))))
            ) ;
        (when (pair? usage-meta)
          (vector-set! stream-state 2 (gemini-map-usage usage-meta))
        ) ;when
        (when (and (vector? candidates) (> (vector-length candidates) 0))
          (let* ((candidate (vector-ref candidates 0))
                 (finish-reason (cdr (or (assoc "finishReason" candidate) '(#f
                                                                            . #f))))
                 (content-obj (cdr (or (assoc "content" candidate) '(#f))))
                 (parts (cdr (or (assoc "parts" content-obj) '(#f . #()))))
                ) ;
            (when finish-reason
              (vector-set! stream-state 4 finish-reason)
            ) ;when
            (when (vector? parts)
              (let loop
                ((i 0))
                (when (< i (vector-length parts))
                  (process-stream-part (vector-ref parts i) stream-state on-event)
                  (loop (+ i 1))
                ) ;when
              ) ;let
            ) ;when
          ) ;let*
        ) ;when
      ) ;let*
    ) ;define

    (define (make-stream-decoder stream-state on-event)
      (make-sse-decoder
        (lambda (sse-ev)
          (let ((raw-data (sse-event-data sse-ev)))
            (when (and (string? raw-data) (not (string-null? raw-data)))
              (let ((json-data (catch #t (lambda () (string->json raw-data)) (lambda args #f))))
                (when json-data
                  (process-sse-chunk-data json-data stream-state on-event)
                ) ;when
              ) ;let
            ) ;when
          ) ;let
        ) ;lambda
      ) ;make-sse-decoder
    ) ;define

  ) ;begin
) ;define-library
