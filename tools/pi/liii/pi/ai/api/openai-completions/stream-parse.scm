;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api openai-completions stream-parse)
  (import (scheme base)
    (liii string)
    (liii json)
    (liii sse)
    (liii pi ai message)
    (liii pi ai api openai-completions core)
    (liii pi ai api openai-completions stream-state)
  ) ;import
  (export append-openai-stream-text process-openai-tool-call-delta
    finish-openai-stream process-openai-chunk make-openai-stream-decoder
    extract-openai-stream-error-message
  ) ;export
  (begin

    (define (%val key obj default-val)
      (let ((p
              (or (assoc key obj)
                (assoc (if (symbol? key) (symbol->string key) (string->symbol key)) obj)
              ) ;or
            ) ;p
           ) ;
        (if (pair? p) (cdr p) default-val)
      ) ;let
    ) ;define

    (define (extract-openai-stream-error-message body)
      (catch #t
        (lambda ()
          (let ((json-data (string->json body)))
            (and (pair? json-data)
              (let ((err (%val "error" json-data #f)))
                (cond ((pair? err) (%val "message" err #f))
                      ((string? err) err)
                      (else #f)
                ) ;cond
              ) ;let
            ) ;and
          ) ;let
        ) ;lambda
        (lambda args #f)
      ) ;catch
    ) ;define

    (define (append-openai-stream-text stream-state text on-event)
      (when (and (string? text) (not (string-null? text)))
        (let* ((blocks (openai-stream-state-blocks stream-state))
               (last-text (openai-stream-state-last-text stream-state))
               (usage (openai-stream-state-usage stream-state))
              ) ;
          (if (not (string? last-text))
            (let ((cur-idx (length blocks)))
              (openai-stream-state-blocks-set! stream-state
                (append blocks (list (pi-text-block text)))
              ) ;openai-stream-state-blocks-set!
              (openai-stream-state-last-text-set! stream-state text)
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
            ) ;let
            (let* ((cur-idx (max 0 (- (length blocks) 1)))
                   (new-text (string-append last-text text))
                   (upd-blocks
                     (let loop
                       ((lst blocks) (acc '()))
                       (if (null? (cdr lst))
                         (append (reverse acc) (list (pi-text-block new-text)))
                         (loop (cdr lst) (cons (car lst) acc))
                       ) ;if
                     ) ;let
                   ) ;upd-blocks
                  ) ;
              (openai-stream-state-blocks-set! stream-state upd-blocks)
              (openai-stream-state-last-text-set! stream-state new-text)
              (when (procedure? on-event)
                (on-event (list (cons 'type "text_delta")
                            (cons 'contentIndex cur-idx)
                            (cons 'delta text)
                            (cons 'usage usage)
                          ) ;list
                ) ;on-event
              ) ;when
            ) ;let*
          ) ;if
        ) ;let*
      ) ;when
    ) ;define

    (define (process-openai-tool-call-delta tc stream-state on-event)
      (let* ((idx (%val "index" tc 0))
             (id (%val "id" tc #f))
             (fn (%val "function" tc '()))
             (name (%val "name" fn #f))
             (args-delta (%val "arguments" fn ""))
             (tools-alist (openai-stream-state-tools stream-state))
             (entry (assoc idx tools-alist))
             (curr-item (if entry (cdr entry) '()))
             (curr-id (or id (%val 'id curr-item "")))
             (curr-name (or name (%val 'name curr-item "")))
             (curr-args (string-append (%val 'args curr-item "") (or args-delta "")))
             (was-emitted? (%val 'emitted curr-item #f))
             (usage (openai-stream-state-usage stream-state))
             (blocks (openai-stream-state-blocks stream-state))
             (cur-idx (+ (length blocks) (length tools-alist)))
             (should-emit-start? (and (not was-emitted?) (not (string-null? curr-name))))
             (upd-item (list (cons 'id curr-id)
                         (cons 'name curr-name)
                         (cons 'args curr-args)
                         (cons 'emitted (or was-emitted? should-emit-start?))
                       ) ;list
             ) ;upd-item
             (new-tools-alist
               (if entry
                 (map
                   (lambda (p) (if (equal? (car p) idx) (cons idx upd-item) p))
                   tools-alist
                 ) ;map
                 (append tools-alist (list (cons idx upd-item)))
               ) ;if
             ) ;new-tools-alist
            ) ;
        (openai-stream-state-tools-set! stream-state new-tools-alist)
        (when (procedure? on-event)
          (when should-emit-start?
            (on-event (list (cons 'type "toolcall_start")
                        (cons 'contentIndex cur-idx)
                        (cons 'id curr-id)
                        (cons 'toolName curr-name)
                        (cons 'usage usage)
                      ) ;list
            ) ;on-event
          ) ;when
          (when (and (string? args-delta) (not (string-null? args-delta)))
            (on-event (list (cons 'type "toolcall_delta")
                        (cons 'contentIndex cur-idx)
                        (cons 'delta args-delta)
                        (cons 'usage usage)
                      ) ;list
            ) ;on-event
          ) ;when
        ) ;when
      ) ;let*
    ) ;define

    (define (finish-openai-stream stream-state on-event)
      (let ((tools-alist (openai-stream-state-tools stream-state))
            (blocks (openai-stream-state-blocks stream-state))
            (usage (openai-stream-state-usage stream-state))
           ) ;
        (when (pair? tools-alist)
          (for-each
            (lambda (p)
              (let* ((item (cdr p))
                     (id (%val 'id item ""))
                     (name (%val 'name item ""))
                     (args-str (%val 'args item "{}"))
                     (args-json
                       (catch #t
                         (lambda () (if (string-null? args-str) '() (string->json args-str)))
                         (lambda args '())
                       ) ;catch
                     ) ;args-json
                     (tc-blk (pi-tool-call-block id name args-json))
                     (cur-idx (length (openai-stream-state-blocks stream-state)))
                    ) ;
                (openai-stream-state-blocks-set! stream-state
                  (append (openai-stream-state-blocks stream-state) (list tc-blk))
                ) ;openai-stream-state-blocks-set!
                (when (procedure? on-event)
                  (on-event (list (cons 'type "toolcall_end")
                              (cons 'contentIndex cur-idx)
                              (cons 'id id)
                              (cons 'toolName name)
                              (cons 'usage usage)
                            ) ;list
                  ) ;on-event
                ) ;when
              ) ;let*
            ) ;lambda
            tools-alist
          ) ;for-each
          (openai-stream-state-stop-reason-set! stream-state "toolUse")
          (openai-stream-state-tools-set! stream-state '())
        ) ;when
      ) ;let
    ) ;define

    (define (process-openai-chunk json-obj stream-state on-event)
      (let* ((top-usage (%val "usage" json-obj #f))
             (choices (%val "choices" json-obj #()))
             (choice
               (if (and (vector? choices) (> (vector-length choices) 0))
                 (vector-ref choices 0)
                 '()
               ) ;if
             ) ;choice
             (choice-usage (%val "usage" choice #f))
             (eff-usage (or top-usage choice-usage))
             (raw-stop (%val "finish_reason" choice #f))
             (delta (%val "delta" choice '()))
            ) ;
        (when (pair? eff-usage)
          (openai-stream-state-usage-set! stream-state (openai-map-usage eff-usage))
        ) ;when
        (when (and (string? raw-stop) (not (string-null? raw-stop)))
          (openai-stream-state-stop-reason-set! stream-state
            (openai-map-stop-reason raw-stop)
          ) ;openai-stream-state-stop-reason-set!
        ) ;when
        (when (pair? delta)
          (let ((content (%val "content" delta #f)) (raw-tc (%val "tool_calls" delta #f)))
            (when (and (string? content) (not (string-null? content)))
              (append-openai-stream-text stream-state content on-event)
            ) ;when
            (when (or (vector? raw-tc) (list? raw-tc))
              (let ((tc-list (if (vector? raw-tc) (vector->list raw-tc) raw-tc)))
                (for-each (lambda (tc) (process-openai-tool-call-delta tc stream-state on-event))
                  tc-list
                ) ;for-each
              ) ;let
            ) ;when
          ) ;let
        ) ;when
      ) ;let*
    ) ;define

    (define (make-openai-stream-decoder stream-state on-event)
      (make-sse-decoder
        (lambda (ev)
          (let ((data (sse-event-data ev)))
            (cond ((not (string? data)) #f)
                  ((string=? (string-trim data) "[DONE]")
                   (finish-openai-stream stream-state on-event)
                  ) ;
                  (else
                    (catch #t
                      (lambda ()
                        (let ((json-obj (string->json data)))
                          (when (pair? json-obj)
                            (process-openai-chunk json-obj stream-state on-event)
                          ) ;when
                        ) ;let
                      ) ;lambda
                      (lambda args #f)
                    ) ;catch
                  ) ;else
            ) ;cond
          ) ;let
        ) ;lambda
      ) ;make-sse-decoder
    ) ;define

  ) ;begin
) ;define-library
