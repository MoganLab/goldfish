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

    (define-record-type <openai-tool-state>
      (%make-openai-tool-state id name args emitted)
      %openai-tool-state?
      (id %openai-tool-state-id)
      (name %openai-tool-state-name)
      (args %openai-tool-state-args)
      (emitted %openai-tool-state-emitted)
    ) ;define-record-type

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

    (define (append-openai-stream-reasoning stream-state reasoning on-event)
      (when (and (string? reasoning) (not (string-null? reasoning)))
        (let* ((last-reasoning (openai-stream-state-last-reasoning stream-state))
               (new-reasoning (if last-reasoning (string-append last-reasoning reasoning) reasoning)
               ) ;new-reasoning
              ) ;
          (openai-stream-state-last-reasoning-set! stream-state new-reasoning)
        ) ;let*
      ) ;when
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
             (curr-item (if entry (cdr entry) (%make-openai-tool-state "" "" "" #f)))
             (curr-id (or id (%openai-tool-state-id curr-item)))
             (curr-name (or name (%openai-tool-state-name curr-item)))
             (curr-args (string-append (%openai-tool-state-args curr-item) (or args-delta ""))
             ) ;curr-args
             (was-emitted? (%openai-tool-state-emitted curr-item))
             (usage (openai-stream-state-usage stream-state))
             (blocks (openai-stream-state-blocks stream-state))
             (cur-idx (+ (length blocks) (length tools-alist)))
             (should-emit-start? (and (not was-emitted?) (not (string-null? curr-name))))
             (upd-item (%make-openai-tool-state curr-id
                         curr-name
                         curr-args
                         (or was-emitted? should-emit-start?)
                       ) ;%make-openai-tool-state
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

    (define (%extract-json-snippet text)
      (let* ((trimmed (string-trim-both text))
             (tc-start (string-contains trimmed "<tool_call>"))
             (tc-end (string-contains trimmed "</tool_call>"))
            ) ;
        (cond
         ((and tc-start tc-end (< tc-start tc-end))
          (string-trim-both (substring trimmed (+ tc-start 11) tc-end))
         ) ;
         ((string-starts? trimmed "```json")
          (let* ((rest (string-trim-both (substring trimmed 7 (string-length trimmed))))
                 (end (string-contains rest "```"))
                ) ;
            (if end (string-trim-both (substring rest 0 end)) rest)
          ) ;let*
         ) ;
         ((string-starts? trimmed "```")
          (let* ((rest (string-trim-both (substring trimmed 3 (string-length trimmed))))
                 (end (string-contains rest "```"))
                ) ;
            (if end (string-trim-both (substring rest 0 end)) rest)
          ) ;let*
         ) ;
         (else
           (let ((b-start (or (string-contains trimmed "{\"name\":")
                            (string-contains trimmed "{\"name\" :")
                            (string-contains trimmed "{\"name\"  :")
                          ) ;or
                 ) ;b-start
                ) ;
             (if b-start
               (let ((r-idx (string-index-right trimmed #\})))
                 (if (and r-idx (>= r-idx b-start))
                   (substring trimmed b-start (+ r-idx 1))
                   trimmed
                 ) ;if
               ) ;let
               trimmed
             ) ;if
           ) ;let
         ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (%parse-xml-tool-call text)
      (let* ((tc-start (string-contains text "<tool_call>"))
             (tc-end (string-contains text "</tool_call>"))
            ) ;
        (if (and tc-start tc-end (< tc-start tc-end))
          (let* ((inner (substring text (+ tc-start 11) tc-end))
                 (json-parsed
                   (catch #t (lambda () (string->json (string-trim-both inner))) (lambda _ #f))
                 ) ;json-parsed
                ) ;
            (if (and (pair? json-parsed) (assoc "name" json-parsed))
              (let* ((name (cdr (assoc "name" json-parsed)))
                     (raw-args (or (pi-user-dict-ref json-parsed "arguments" #f)
                                 (pi-user-dict-ref json-parsed "parameters" '())
                               ) ;or
                     ) ;raw-args
                    ) ;
                (cons name raw-args)
              ) ;let*
              (let ((fn-start (string-contains inner "<function=")))
                (if fn-start
                  (let* ((after-fn (+ fn-start 10)) (fn-end (string-contains inner ">" after-fn)))
                    (if fn-end
                      (let* ((fn-name (string-trim-both (substring inner after-fn fn-end)))
                             (args
                               (let loop
                                 ((pos fn-end) (params '()))
                                 (let ((p-start (string-contains inner "<parameter=" pos)))
                                   (if (not p-start)
                                     (reverse params)
                                     (let* ((p-name-start (+ p-start 11))
                                            (p-name-end (string-contains inner ">" p-name-start))
                                           ) ;
                                       (if (not p-name-end)
                                         (reverse params)
                                         (let* ((p-name (string-trim-both (substring inner p-name-start p-name-end)))
                                                (val-start (+ p-name-end 1))
                                                (val-end (string-contains inner "</parameter>" val-start))
                                               ) ;
                                           (if (not val-end)
                                             (reverse params)
                                             (let* ((val-raw (string-trim-both (substring inner val-start val-end)))
                                                    (num-val (string->number val-raw))
                                                    (val (if (number? num-val) num-val val-raw))
                                                   ) ;
                                               (loop (+ val-end 12) (cons (cons p-name val) params))
                                             ) ;let*
                                           ) ;if
                                         ) ;let*
                                       ) ;if
                                     ) ;let*
                                   ) ;if
                                 ) ;let
                               ) ;let
                             ) ;args
                            ) ;
                        (cons fn-name args)
                      ) ;let*
                      #f
                    ) ;if
                  ) ;let*
                  #f
                ) ;if
              ) ;let
            ) ;if
          ) ;let*
          #f
        ) ;if
      ) ;let*
    ) ;define

    (define (%parse-any-tool-call text)
      (and (string? text)
        (or (%parse-xml-tool-call text)
          (let* ((snippet (%extract-json-snippet text))
                 (parsed (catch #t (lambda () (string->json snippet)) (lambda args #f)))
                ) ;
            (and (pair? parsed)
              (assoc "name" parsed)
              (let* ((name (cdr (assoc "name" parsed)))
                     (raw-args
                       (let ((p (or (assoc "arguments" parsed) (assoc "parameters" parsed))))
                         (if p (cdr p) '())
                       ) ;let
                     ) ;raw-args
                     (args
                       (cond ((pair? raw-args) raw-args)
                             ((string? raw-args)
                              (let ((sub (catch #t (lambda () (string->json raw-args)) (lambda _ #f))))
                                (cond ((pair? sub) sub)
                                      ((equal? name "bash") (list (cons "command" raw-args)))
                                      (else (list (cons "input" raw-args)))
                                ) ;cond
                              ) ;let
                             ) ;
                             (else '())
                       ) ;cond
                     ) ;args
                    ) ;
                (cons name args)
              ) ;let*
            ) ;and
          ) ;let*
        ) ;or
      ) ;and
    ) ;define

    (define (%maybe-extract-text-tool-call! stream-state on-event)
      (let* ((blocks (openai-stream-state-blocks stream-state))
             (last-text (openai-stream-state-last-text stream-state))
             (last-reasoning (openai-stream-state-last-reasoning stream-state))
             (tc (or (%parse-any-tool-call last-text) (%parse-any-tool-call last-reasoning)))
            ) ;
        (cond
          (tc
            (let* ((name (car tc))
                   (args (cdr tc))
                   (call-id "call_inline_1")
                   (tc-blk (pi-tool-call-block call-id name args))
                   (usage (openai-stream-state-usage stream-state))
                  ) ;
              (openai-stream-state-blocks-set! stream-state (list tc-blk))
              (openai-stream-state-stop-reason-set! stream-state "toolUse")
              (when (procedure? on-event)
                (on-event (list (cons 'type "toolcall_start")
                            (cons 'contentIndex 0)
                            (cons 'id call-id)
                            (cons 'toolName name)
                            (cons 'usage usage)
                          ) ;list
                ) ;on-event
              ) ;when
              (when (procedure? on-event)
                (on-event (list (cons 'type "toolcall_end")
                            (cons 'contentIndex 0)
                            (cons 'id call-id)
                            (cons 'toolName name)
                            (cons 'usage usage)
                          ) ;list
                ) ;on-event
              ) ;when
            ) ;let*
          ) ;tc
          ((and (null? blocks)
             (string? last-reasoning)
             (not (string-null? (string-trim-both last-reasoning)))
           ) ;and
           (let* ((clean-text (string-trim-both last-reasoning))
                  (txt-blk (pi-text-block clean-text))
                 ) ;
             (openai-stream-state-blocks-set! stream-state (list txt-blk))
             (openai-stream-state-last-text-set! stream-state clean-text)
           ) ;let*
          ) ;
          (else #f)
        ) ;cond
      ) ;let*
    ) ;define

    (define (finish-openai-stream stream-state on-event)
      (let ((tools-alist (openai-stream-state-tools stream-state))
            (blocks (openai-stream-state-blocks stream-state))
            (usage (openai-stream-state-usage stream-state))
           ) ;
        (if (pair? tools-alist)
          (begin
            (for-each
              (lambda (p)
                (let* ((item (cdr p))
                       (id (%openai-tool-state-id item))
                       (name (%openai-tool-state-name item))
                       (args-str (%openai-tool-state-args item))
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
          ) ;begin
          (%maybe-extract-text-tool-call! stream-state on-event)
        ) ;if
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
          (let ((content (%val "content" delta #f))
                (reasoning (or (%val "reasoning" delta #f) (%val "reasoning_content" delta #f)))
                (raw-tc (%val "tool_calls" delta #f))
               ) ;
            (when (and (string? content) (not (string-null? content)))
              (append-openai-stream-text stream-state content on-event)
            ) ;when
            (when (and (string? reasoning) (not (string-null? reasoning)))
              (append-openai-stream-reasoning stream-state reasoning on-event)
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
