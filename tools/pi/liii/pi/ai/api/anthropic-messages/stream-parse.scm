;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api anthropic-messages stream-parse)
  (import (scheme base)
    (liii string)
    (liii json)
    (liii sse)
    (liii pi ai message)
    (liii pi ai api anthropic-messages core)
    (liii pi ai api anthropic-messages stream-state)
    (liii pi ai api anthropic-messages thinking)
  ) ;import
  (export append-anthropic-stream-text handle-anthropic-delta
    stop-anthropic-current-block process-anthropic-sse
    make-anthropic-stream-decoder
  ) ;export
  (begin

    (define-record-type <anthropic-tool-state>
      (%make-anthropic-tool-state id name json index)
      %anthropic-tool-state?
      (id %anthropic-tool-state-id)
      (name %anthropic-tool-state-name)
      (json %anthropic-tool-state-json)
      (index %anthropic-tool-state-index)
    ) ;define-record-type

    (define (%emit-delta on-ev cur-idx text usage stream-state)
      (when (procedure? on-ev)
        (when
          (zero? (string-length (or (anthropic-stream-state-last-text stream-state) "")))
          (on-ev (list (cons 'type "text_start")
                   (cons 'contentIndex cur-idx)
                   (cons 'usage usage)
                 ) ;list
          ) ;on-ev
        ) ;when
        (on-ev (list (cons 'type "text_delta")
                 (cons 'contentIndex cur-idx)
                 (cons 'delta text)
                 (cons 'usage usage)
               ) ;list
        ) ;on-ev
      ) ;when
    ) ;define

    (define (append-anthropic-stream-text stream-state t on-ev)
      (let* ((blocks (anthropic-stream-state-blocks stream-state))
             (last-text (anthropic-stream-state-last-text stream-state))
             (usage (anthropic-stream-state-usage stream-state))
             (cur-idx
               (if (string? last-text) (max 0 (- (length blocks) 1)) (length blocks))
             ) ;cur-idx
            ) ;
        (%emit-delta on-ev cur-idx t usage stream-state)
        (if (not (string? last-text))
          (begin
            (anthropic-stream-state-blocks-set! stream-state
              (append blocks (list (pi-text-block t)))
            ) ;anthropic-stream-state-blocks-set!
            (anthropic-stream-state-last-text-set! stream-state t)
          ) ;begin
          (let* ((new-t (string-append last-text t))
                 (upd
                   (let loop
                     ((lst blocks) (acc '()))
                     (if (null? (cdr lst))
                       (reverse (cons (pi-text-block new-t) acc))
                       (loop (cdr lst) (cons (car lst) acc))
                     ) ;if
                   ) ;let
                 ) ;upd
                ) ;
            (anthropic-stream-state-blocks-set! stream-state upd)
            (anthropic-stream-state-last-text-set! stream-state new-t)
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (%handle-json-delta delta idx stream-state on-ev)
      (let* ((pj (cdr (or (assoc "partial_json" delta) '(#f . ""))))
             (cur-tool (anthropic-stream-state-current-tool stream-state))
            ) ;
        (when (%anthropic-tool-state? cur-tool)
          (let* ((cur-json (%anthropic-tool-state-json cur-tool))
                 (upd-tool (%make-anthropic-tool-state (%anthropic-tool-state-id cur-tool)
                             (%anthropic-tool-state-name cur-tool)
                             (string-append cur-json pj)
                             idx
                           ) ;%make-anthropic-tool-state
                 ) ;upd-tool
                ) ;
            (anthropic-stream-state-current-tool-set! stream-state upd-tool)
            (when (procedure? on-ev)
              (on-ev (list (cons 'type "toolcall_delta")
                       (cons 'contentIndex idx)
                       (cons 'delta pj)
                       (cons 'usage (anthropic-stream-state-usage stream-state))
                     ) ;list
              ) ;on-ev
            ) ;when
          ) ;let*
        ) ;when
      ) ;let*
    ) ;define

    (define (handle-anthropic-delta json-obj stream-state on-ev)
      (let* ((idx (cdr (or (assoc "index" json-obj) '(#f . 0))))
             (delta (cdr (or (assoc "delta" json-obj) '(#f #_quote ()))))
             (dtype (cdr (or (assoc "type" delta) '(#f . ""))))
            ) ;
        (cond
         ((equal? dtype "text_delta")
          (append-anthropic-stream-text stream-state
            (cdr (or (assoc "text" delta) '(#f . "")))
            on-ev
          ) ;append-anthropic-stream-text
         ) ;
         ((equal? dtype "input_json_delta")
          (%handle-json-delta delta idx stream-state on-ev)
         ) ;
         ((or (equal? dtype "thinking_delta") (equal? dtype "signature_delta"))
          (anthropic-handle-thinking-delta delta idx stream-state on-ev)
         ) ;
        ) ;cond
      ) ;let*
    ) ;define

    (define (%finish-tool-block cur-tool blocks usage stream-state on-ev)
      (let* ((id (%anthropic-tool-state-id cur-tool))
             (name (%anthropic-tool-state-name cur-tool))
             (json-str (%anthropic-tool-state-json cur-tool))
             (args (anthropic-parse-tool-arguments json-str))
             (blk (pi-tool-call-block id name args))
            ) ;
        (anthropic-stream-state-blocks-set! stream-state (append blocks (list blk)))
        (anthropic-stream-state-tool-calls-set! stream-state
          (append (anthropic-stream-state-tool-calls stream-state) (list blk))
        ) ;anthropic-stream-state-tool-calls-set!
        (anthropic-stream-state-current-tool-set! stream-state #f)
        (when (procedure? on-ev)
          (on-ev (list (cons 'type "toolcall_end")
                   (cons 'contentIndex (%anthropic-tool-state-index cur-tool))
                   (cons 'id id)
                   (cons 'name name)
                   (cons 'arguments args)
                   (cons 'usage usage)
                 ) ;list
          ) ;on-ev
        ) ;when
      ) ;let*
    ) ;define

    (define (stop-anthropic-current-block stream-state on-ev)
      (let ((cur-tool (anthropic-stream-state-current-tool stream-state))
            (cur-th (anthropic-stream-state-current-thinking stream-state))
            (last-text (anthropic-stream-state-last-text stream-state))
            (blocks (anthropic-stream-state-blocks stream-state))
            (usage (anthropic-stream-state-usage stream-state))
           ) ;
        (cond ((%anthropic-tool-state? cur-tool)
               (%finish-tool-block cur-tool blocks usage stream-state on-ev)
              ) ;
              (cur-th (anthropic-finish-thinking-block stream-state on-ev))
              ((string? last-text)
               (anthropic-stream-state-last-text-set! stream-state #f)
               (when (procedure? on-ev)
                 (on-ev
                   (list (cons 'type "text_end")
                     (cons 'contentIndex (- (length blocks) 1))
                     (cons 'content last-text)
                     (cons 'usage usage)
                   ) ;list
                 ) ;on-ev
               ) ;when
              ) ;
        ) ;cond
      ) ;let
    ) ;define

    (define (%handle-block-start json-obj stream-state on-ev)
      (let* ((idx (cdr (or (assoc "index" json-obj) '(#f . 0))))
             (cb (cdr (or (assoc "content_block" json-obj) '(#f #_quote ()))))
             (cb-type (cdr (or (assoc "type" cb) '(#f . ""))))
            ) ;
        (cond
         ((equal? cb-type "tool_use")
          (let ((id (cdr (or (assoc "id" cb) '(#f . ""))))
                (name (cdr (or (assoc "name" cb) '(#f . ""))))
               ) ;
            (anthropic-stream-state-current-tool-set! stream-state
              (%make-anthropic-tool-state id name "" idx)
            ) ;anthropic-stream-state-current-tool-set!
            (when (procedure? on-ev)
              (on-ev (list (cons 'type "toolcall_start")
                       (cons 'contentIndex idx)
                       (cons 'id id)
                       (cons 'toolName name)
                       (cons 'usage (anthropic-stream-state-usage stream-state))
                     ) ;list
              ) ;on-ev
            ) ;when
          ) ;let
         ) ;
         ((equal? cb-type "thinking")
          (anthropic-start-thinking-block stream-state idx on-ev)
         ) ;
         (else (anthropic-stream-state-last-text-set! stream-state #f))
        ) ;cond
      ) ;let*
    ) ;define

    (define (%usage-num u cur-u str-k sym-k)
      (let ((p (or (assoc str-k u) (assoc sym-k u))))
        (if p (cdr p) (cdr (or (assoc sym-k cur-u) '(#f . 0))))
      ) ;let
    ) ;define

    (define (%handle-msg-delta json-obj stream-state)
      (let* ((delta (cdr (or (assoc "delta" json-obj) '(#f #_quote ()))))
             (sr (cdr (or (assoc "stop_reason" delta) '(#f . #f))))
             (u (cdr (or (assoc "usage" json-obj) '(#f #_quote ()))))
            ) ;
        (when sr
          (anthropic-stream-state-stop-reason-set! stream-state sr)
        ) ;when
        (when (pair? u)
          (let* ((cur-u (anthropic-stream-state-usage stream-state))
                 (out-tok (%usage-num u cur-u "output_tokens" 'output))
                 (in-tok (%usage-num u cur-u "input_tokens" 'input))
                 (c-read (%usage-num u cur-u "cache_read_input_tokens" 'cacheRead))
                 (c-write (%usage-num u cur-u "cache_creation_input_tokens" 'cacheWrite))
                ) ;
            (anthropic-stream-state-usage-set! stream-state
              (list (cons 'input in-tok)
                (cons 'output out-tok)
                (cons 'cacheRead c-read)
                (cons 'cacheWrite c-write)
                (cons 'totalTokens (+ in-tok out-tok))
              ) ;list
            ) ;anthropic-stream-state-usage-set!
          ) ;let*
        ) ;when
      ) ;let*
    ) ;define

    (define (process-anthropic-sse json-obj stream-state on-ev)
      (let ((ev-type (cdr (or (assoc "type" json-obj) '(#f . "")))))
        (cond
         ((equal? ev-type "message_start")
          (let* ((msg (cdr (or (assoc "message" json-obj) '(#f #_quote ()))))
                 (u (cdr (or (assoc "usage" msg) '(#f #_quote ()))))
                ) ;
            (anthropic-stream-state-usage-set! stream-state (anthropic-map-usage u))
          ) ;let*
         ) ;
         ((equal? ev-type "content_block_start")
          (%handle-block-start json-obj stream-state on-ev)
         ) ;
         ((equal? ev-type "content_block_delta")
          (handle-anthropic-delta json-obj stream-state on-ev)
         ) ;
         ((equal? ev-type "content_block_stop")
          (stop-anthropic-current-block stream-state on-ev)
         ) ;
         ((equal? ev-type "message_delta") (%handle-msg-delta json-obj stream-state))
         ((equal? ev-type "error")
          (anthropic-stream-state-stop-reason-set! stream-state "error")
         ) ;
        ) ;cond
      ) ;let
    ) ;define

    (define (make-anthropic-stream-decoder stream-state on-ev)
      (make-sse-decoder
        (lambda (sse-ev)
          (let ((data (sse-event-data sse-ev)))
            (when (and (string? data) (not (string-null? data)))
              (let ((obj (catch #t (lambda () (string->json data)) (lambda _ #f))))
                (when obj
                  (process-anthropic-sse obj stream-state on-ev)
                ) ;when
              ) ;let
            ) ;when
          ) ;let
        ) ;lambda
      ) ;make-sse-decoder
    ) ;define

  ) ;begin
) ;define-library
