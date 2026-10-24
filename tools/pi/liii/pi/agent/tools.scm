;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi agent tools)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii pi ai event)
    (liii pi tools)
    (liii pi ai message)
    (liii pi ai tools types)
    (liii pi ai tools record)
    (liii pi extension types)
    (liii pi extension registry)
    (liii pi extension event)
  ) ;import
  (export pi-agent-assoc-val pi-first-block-from-content
    pi-extract-tool-result-text pi-make-tool-result-msg pi-execute-one-tool-call
    pi-execute-one-tool-call-with-status pi-execute-tool-calls
    pi-execute-tool-calls-with-status pi-fail-tool-calls-length
    pi-filter-tool-calls pi-extract-tool-result-blocks
  ) ;export
  (begin

    (define (pi-agent-assoc-val key alt-key obj default-val)
      (let ((pair (or (assoc key obj) (assoc alt-key obj))))
        (if (pair? pair) (cdr pair) default-val)
      ) ;let
    ) ;define

    (define (pi-first-block-from-content c)
      (cond
       ((and (vector? c) (> (vector-length c) 0)) (vector-ref c 0))
       ((and (pair? c) (or (pair? (car c)) (pi-block? (car c)))) (car c))
       (else '())
      ) ;cond
    ) ;define

    (define (pi-extract-tool-result-text res)
      (let* ((c (pi-agent-assoc-val 'content "content" res #()))
             (first-blk (pi-first-block-from-content c))
            ) ;
        (pi-agent-assoc-val 'text "text" first-blk "")
      ) ;let*
    ) ;define

    (define (pi-make-tool-result-msg id name text is-err)
      (pi-tool-result-message id name (list (pi-text-block text)) is-err)
    ) ;define

    (define (%log-stderr msg)
      (let ((p (if (defined? '*stderr*) *stderr* (current-error-port))))
        (display msg p)
        (newline p)
      ) ;let
    ) ;define

    (define (%active-registry opt-config)
      (cond ((extension-registry? opt-config) opt-config)
            ((and (pair? opt-config) (assoc 'registry opt-config))
             (cdr (assoc 'registry opt-config))
            ) ;
            ((current-extension-registry) => (lambda (r) r))
            (else (default-extension-registry))
      ) ;cond
    ) ;define

    (define* (pi-execute-one-tool-call-with-status call emit timeout (opt-config #f))
      (let* ((id (pi-agent-assoc-val 'id "id" call ""))
             (name (pi-agent-assoc-val 'name "name" call ""))
             (args (pi-agent-assoc-val 'arguments "arguments" call '()))
             (reg (%active-registry opt-config))
            ) ;
        (when (procedure? emit)
          (emit 'tool_execution_start id name args)
        ) ;when
        (when (and (not (procedure? emit)) (equal? name "web_search"))
          (let ((q (pi-agent-assoc-val 'query "query" args "")))
            (when
              (and (string? q) (not (string-null? (string-trim-both q))))
              (%log-stderr (string-append "Searching web: " q "..."))
            ) ;when
          ) ;let
        ) ;when
        (let* ((call-payload (list (cons 'id id) (cons 'name name) (cons 'tool name) (cons 'input args))
               ) ;call-payload
               (emit-res (pi-emit! reg 'tool_call call-payload))
               (action (cdr (or (assq 'action emit-res) '(#f . continue))))
              ) ;
          (if (eq? action 'block)
            (let* ((reason (cdr (or (assq 'reason emit-res) '(#f
                                                              . "Tool execution blocked by extension")))
                   ) ;reason
                   (term? (cdr (or (assq 'terminate emit-res) '(#f . #f))))
                   (raw-res
                     (if term?
                       (list
                         (cons 'content (vector (list (cons 'type "text") (cons 'text reason))))
                         (cons 'isError #t)
                         (cons 'terminate #t)
                       ) ;list
                       (pi-tool-result reason #t)
                     ) ;if
                   ) ;raw-res
                   (tool-msg (pi-make-tool-result-msg id name reason #t))
                   (res-payload (list (cons 'id id)
                                  (cons 'name name)
                                  (cons 'tool name)
                                  (cons 'input args)
                                  (cons 'result reason)
                                  (cons 'isError #t)
                                ) ;list
                   ) ;res-payload
                  ) ;
              (pi-emit! reg 'tool_result res-payload)
              (when (procedure? emit)
                (emit 'tool_execution_end id name raw-res #t)
                (emit 'message_start tool-msg)
                (emit 'message_end tool-msg)
              ) ;when
              (values tool-msg (if term? #t #f))
            ) ;let*
            (let* ((eff-payload
                     (cdr (or (assq 'payload emit-res) (list (cons 'input args))))
                   ) ;eff-payload
                   (effective-args (cdr (or (assq 'input eff-payload) (cons 'input args))))
                   (raw-res (if opt-config
                              (apply execute-tool-call
                                name
                                effective-args
                                :timeout timeout
                                (list :config opt-config)
                              ) ;apply
                              (execute-tool-call name effective-args :timeout timeout)
                            ) ;if
                   ) ;raw-res
                   (is-err? (if (pi-tool-result? raw-res)
                              (pi-tool-result-is-error raw-res)
                              (pi-agent-assoc-val 'isError "isError" raw-res #f)
                            ) ;if
                   ) ;is-err?
                   (text (pi-extract-tool-result-text raw-res))
                   (tool-msg (pi-make-tool-result-msg id name text is-err?))
                   (res-payload (list (cons 'id id)
                                  (cons 'name name)
                                  (cons 'tool name)
                                  (cons 'input effective-args)
                                  (cons 'result text)
                                  (cons 'isError is-err?)
                                ) ;list
                   ) ;res-payload
                  ) ;
              (pi-emit! reg 'tool_result res-payload)
              (when (procedure? emit)
                (emit 'tool_execution_end id name raw-res is-err?)
                (emit 'message_start tool-msg)
                (emit 'message_end tool-msg)
              ) ;when
              (values tool-msg #f)
            ) ;let*
          ) ;if
        ) ;let*
      ) ;let*
    ) ;define*

    (define* (pi-execute-one-tool-call call emit timeout (opt-config #f))
      (let-values (((tool-msg term?)
                    (pi-execute-one-tool-call-with-status call emit timeout opt-config)
                   ) ;
                  ) ;
        tool-msg
      ) ;let-values
    ) ;define*

    (define* (pi-execute-tool-calls-with-status tool-calls emit timeout (opt-config #f))
      (let loop
        ((rest tool-calls) (acc '()) (batch-term? #f))
        (if (null? rest)
          (values (reverse acc) batch-term?)
          (if batch-term?
            (let* ((call (car rest))
                   (id (pi-agent-assoc-val 'id "id" call ""))
                   (name (pi-agent-assoc-val 'name "name" call ""))
                   (skip-text "Tool call skipped: previous tool call terminated the run")
                   (raw-res (pi-tool-result skip-text #t))
                   (tool-msg (pi-make-tool-result-msg id name skip-text #t))
                  ) ;
              (when (procedure? emit)
                (emit 'tool_execution_start id name '())
                (emit 'tool_execution_end id name raw-res #t)
                (emit 'message_start tool-msg)
                (emit 'message_end tool-msg)
              ) ;when
              (loop (cdr rest) (cons tool-msg acc) #t)
            ) ;let*
            (let-values (((msg term?)
                          (pi-execute-one-tool-call-with-status (car rest) emit timeout opt-config)
                         ) ;
                        ) ;
              (loop (cdr rest) (cons msg acc) term?)
            ) ;let-values
          ) ;if
        ) ;if
      ) ;let
    ) ;define*

    (define* (pi-execute-tool-calls tool-calls emit timeout (opt-config #f))
      (let-values (((res term?)
                    (pi-execute-tool-calls-with-status tool-calls emit timeout opt-config)
                   ) ;
                  ) ;
        res
      ) ;let-values
    ) ;define*

    (define (pi-fail-tool-calls-length tool-calls emit)
      (map
        (lambda (call)
          (let* ((id (pi-agent-assoc-val 'id "id" call ""))
                 (name (pi-agent-assoc-val 'name "name" call ""))
                 (tool-msg (pi-make-tool-result-msg id name
                             "Tool call skipped: model output truncated by token limit"
                             #t
                           ) ;pi-make-tool-result-msg
                 ) ;tool-msg
                ) ;
            (when (procedure? emit)
              (emit 'message_start tool-msg)
              (emit 'message_end tool-msg)
            ) ;when
            tool-msg
          ) ;let*
        ) ;lambda
        tool-calls
      ) ;map
    ) ;define

    (define (pi-filter-tool-calls content)
      (filter (lambda (c) (equal? (pi-agent-assoc-val 'type "type" c "") "toolCall"))
        content
      ) ;filter
    ) ;define

    (define (pi-extract-tool-result-blocks msgs count)
      (let* ((tail (list-tail msgs (- (length msgs) count))))
        (list->vector
          (map
            (lambda (m)
              (let* ((id (pi-agent-assoc-val 'toolCallId "toolCallId" m ""))
                     (name (pi-agent-assoc-val 'toolName "toolName" m ""))
                     (c (pi-agent-assoc-val 'content "content" m '()))
                     (text (pi-agent-assoc-val 'text "text" (pi-first-block-from-content c) ""))
                     (is-err? (pi-agent-assoc-val 'isError "isError" m #f))
                    ) ;
                (pi-tool-result-message id name (list (pi-text-block text)) is-err?)
              ) ;let*
            ) ;lambda
            tail
          ) ;map
        ) ;list->vector
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
