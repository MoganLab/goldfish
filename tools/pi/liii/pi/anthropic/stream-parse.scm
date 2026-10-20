;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi anthropic stream-parse)
  (import (scheme base)
    (liii string)
    (liii json)
    (liii sse)
    (liii pi message)
    (liii pi anthropic core)
    (liii pi anthropic thinking)
  ) ;import
  (export append-anthropic-stream-text handle-anthropic-delta
    stop-anthropic-current-block process-anthropic-sse
    make-anthropic-stream-decoder
  ) ;export
  (begin

    (define (%emit-delta on-ev cur-idx text usage stream-state)
      (when (procedure? on-ev)
        (when
          (zero? (string-length (or (vector-ref stream-state 1) "")))
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
      (let* ((blocks (vector-ref stream-state 0))
             (last-text (vector-ref stream-state 1))
             (usage (vector-ref stream-state 2))
             (cur-idx
               (if (string? last-text) (max 0 (- (length blocks) 1)) (length blocks))
             ) ;cur-idx
            ) ;
        (%emit-delta on-ev cur-idx t usage stream-state)
        (if (not (string? last-text))
          (begin
            (vector-set! stream-state 0 (append blocks (list (pi-text-block t))))
            (vector-set! stream-state 1 t)
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
            (vector-set! stream-state 0 upd)
            (vector-set! stream-state 1 new-t)
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (%handle-json-delta delta idx stream-state on-ev)
      (let* ((pj (cdr (or (assoc "partial_json" delta) '(#f . ""))))
             (cur-tool (vector-ref stream-state 5))
            ) ;
        (when (pair? cur-tool)
          (let* ((cur-json (cdr (assoc 'json cur-tool)))
                 (upd-tool
                   (list (cons 'id (cdr (assoc 'id cur-tool)))
                     (cons 'name (cdr (assoc 'name cur-tool)))
                     (cons 'json (string-append cur-json pj))
                     (cons 'index idx)
                   ) ;list
                 ) ;upd-tool
                ) ;
            (vector-set! stream-state 5 upd-tool)
            (when (procedure? on-ev)
              (on-ev (list (cons 'type "toolcall_delta")
                       (cons 'contentIndex idx)
                       (cons 'delta pj)
                       (cons 'usage (vector-ref stream-state 2))
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
      (let* ((id (cdr (assoc 'id cur-tool)))
             (name (cdr (assoc 'name cur-tool)))
             (json-str (cdr (assoc 'json cur-tool)))
             (args (anthropic-parse-tool-arguments json-str))
             (blk (pi-tool-call-block id name args))
            ) ;
        (vector-set! stream-state 0 (append blocks (list blk)))
        (vector-set! stream-state 3 (append (vector-ref stream-state 3) (list blk)))
        (vector-set! stream-state 5 #f)
        (when (procedure? on-ev)
          (on-ev
            (list (cons 'type "toolcall_end")
              (cons 'contentIndex (cdr (assoc 'index cur-tool)))
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
      (let ((cur-tool (vector-ref stream-state 5))
            (cur-th (vector-ref stream-state 6))
            (last-text (vector-ref stream-state 1))
            (blocks (vector-ref stream-state 0))
            (usage (vector-ref stream-state 2))
           ) ;
        (cond ((pair? cur-tool) (%finish-tool-block cur-tool blocks usage stream-state on-ev))
              ((pair? cur-th) (anthropic-finish-thinking-block stream-state on-ev))
              ((string? last-text)
               (vector-set! stream-state 1 #f)
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
            (vector-set! stream-state
              5
              (list (cons 'id id) (cons 'name name) (cons 'json "") (cons 'index idx))
            ) ;vector-set!
            (when (procedure? on-ev)
              (on-ev (list (cons 'type "toolcall_start")
                       (cons 'contentIndex idx)
                       (cons 'id id)
                       (cons 'toolName name)
                       (cons 'usage (vector-ref stream-state 2))
                     ) ;list
              ) ;on-ev
            ) ;when
          ) ;let
         ) ;
         ((equal? cb-type "thinking")
          (anthropic-start-thinking-block stream-state idx on-ev)
         ) ;
         (else (vector-set! stream-state 1 #f))
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
          (vector-set! stream-state 4 sr)
        ) ;when
        (when (pair? u)
          (let* ((cur-u (vector-ref stream-state 2))
                 (out-tok (%usage-num u cur-u "output_tokens" 'output))
                 (in-tok (%usage-num u cur-u "input_tokens" 'input))
                 (c-read (%usage-num u cur-u "cache_read_input_tokens" 'cacheRead))
                 (c-write (%usage-num u cur-u "cache_creation_input_tokens" 'cacheWrite))
                ) ;
            (vector-set! stream-state
              2
              (list (cons 'input in-tok)
                (cons 'output out-tok)
                (cons 'cacheRead c-read)
                (cons 'cacheWrite c-write)
                (cons 'totalTokens (+ in-tok out-tok))
              ) ;list
            ) ;vector-set!
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
            (vector-set! stream-state 2 (anthropic-map-usage u))
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
         ((equal? ev-type "error") (vector-set! stream-state 4 "error"))
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
