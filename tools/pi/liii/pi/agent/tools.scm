;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi agent tools)
  (import (scheme base)
    (scheme time)
    (liii string)
    (liii list)
    (liii pi ai abort)
    (liii pi ai config runtime)
    (liii pi ai event)
    (liii pi ai message)
    (liii pi ai tools types)
    (liii pi ai tools record)
  ) ;import
  (export pi-agent-assoc-val pi-first-block-from-content
    pi-extract-tool-result-text pi-make-tool-result-msg pi-execute-one-tool-call
    pi-execute-one-tool-call-with-status pi-execute-tool-calls
    pi-execute-tool-calls-with-status pi-fail-tool-calls-length
    pi-filter-tool-calls pi-extract-tool-result-blocks extract-assistant-text
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
      (let* ((tr (or (json->pi-tool-result res) res))
             (c (cond ((pi-tool-result? tr) (pi-tool-result-content tr))
                      ((pi-tool-result-message? tr) (pi-tool-result-message-content tr))
                      ((pair? tr) (pi-agent-assoc-val 'content "content" tr #()))
                      (else #())
                ) ;cond
             ) ;c
             (first-blk (pi-first-block-from-content c))
             (blk (or (json->pi-block first-blk) first-blk))
            ) ;
        (if (pi-text-block? blk)
          (pi-text-block-text blk)
          (pi-agent-assoc-val 'text "text" first-blk "")
        ) ;if
      ) ;let*
    ) ;define

    (define (%normalize-one-block b)
      (or (json->pi-block b) b)
    ) ;define

    (define (%to-block-list c)
      (let ((lst
              (cond ((vector? c) (vector->list c))
                    ((pair? c) c)
                    ((string? c) (list (pi-text-block c)))
                    (else '())
              ) ;cond
            ) ;lst
           ) ;
        (map %normalize-one-block lst)
      ) ;let
    ) ;define

    (define (%extract-tool-result-content-blocks res)
      (let ((tr (or (json->pi-tool-result res) res)))
        (%to-block-list (cond ((pi-tool-result? tr) (pi-tool-result-content tr))
                              ((pair? tr) (pi-agent-assoc-val 'content "content" tr #()))
                              (else tr)
                        ) ;cond
        ) ;%to-block-list
      ) ;let
    ) ;define

    (define (pi-make-tool-result-msg id name content is-err)
      (let ((blks
              (if
                (and (pair? content)
                  (not (pi-block? (car content)))
                  (not (pair? (car content)))
                ) ;and
                (list (%normalize-one-block content))
                (%to-block-list content)
              ) ;if
            ) ;blks
           ) ;
        (pi-tool-result-message id name blks is-err)
      ) ;let
    ) ;define

    (define (%log-stderr msg)
      (let ((p (if (defined? '*stderr*) *stderr* (current-error-port))))
        (display msg p)
        (newline p)
      ) ;let
    ) ;define

    (define (%tool-executor opt-config)
      (let ((val (pi-cfg-ref opt-config 'execute-tool-call)))
        (or (and (procedure? val) val)
          (lambda (name . args) (pi-tool-result "execute-tool-call not configured" #t))
        ) ;or
      ) ;let
    ) ;define
    (define (%collect-text-blocks blks)
      (map pi-text-block-text (filter pi-text-block? (%to-block-list blks)))
    ) ;define
    (define (extract-assistant-text msg)
      (let* ((m (or (json->pi-message msg) msg))
             (content (cond ((pi-assistant-message? m) (pi-assistant-message-content m))
                            ((pi-user-message? m) (pi-user-message-content m))
                            ((pi-tool-result-message? m) (pi-tool-result-message-content m))
                            (else (pi-agent-assoc-val 'content "content" m '()))
                      ) ;cond
             ) ;content
             (texts (%collect-text-blocks content))
            ) ;
        (string-join texts "\n")
      ) ;let*
    ) ;define

    (define (%tool-call-fields call)
      (let ((c (or (json->pi-block call) call)))
        (if (pi-tool-call-block? c)
          (values (pi-tool-call-block-id c)
            (pi-tool-call-block-name c)
            (pi-tool-call-block-arguments c)
          ) ;values
          (values (pi-agent-assoc-val 'id "id" call "")
            (pi-agent-assoc-val 'name "name" call "")
            (pi-agent-assoc-val 'arguments "arguments" call '())
          ) ;values
        ) ;if
      ) ;let
    ) ;define

    (define (%handle-blocked-tool-call id name args emit-res reg-emit emit)
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
        (when reg-emit
          (reg-emit 'tool_result res-payload)
        ) ;when
        (when (procedure? emit)
          (emit 'tool_execution_end id name raw-res #t)
          (emit 'message_start tool-msg)
          (emit 'message_end tool-msg)
        ) ;when
        (values tool-msg (if term? #t #f))
      ) ;let*
    ) ;define

    (define (%run-tool-call id name args emit-res reg-emit exec timeout opt-config emit)
      (let* ((eff-payload
               (cdr (or (assq 'payload emit-res) (list (cons 'input args))))
             ) ;eff-payload
             (effective-args (cdr (or (assq 'input eff-payload) (cons 'input args))))
             (on-update
               (lambda (text)
                 (when (procedure? emit)
                   (emit 'tool_execution_update id name effective-args (pi-tool-result text #f))
                 ) ;when
               ) ;lambda
             ) ;on-update
             (exec-opts
               (append
                 (if (and (number? timeout) (> timeout 0)) `(,:timeout ,timeout) '())
                 (if (equal? name "bash")
                   `(,:on-update ,on-update ,:check-abort ,pi-aborted?)
                   '()
                 ) ;if
                 (if (pair? opt-config) `(,:config ,opt-config) '())
               ) ;append
             ) ;exec-opts
             (started-at (current-jiffy))
             (raw-res (apply exec name effective-args exec-opts))
             (duration-ms
               (inexact->exact
                 (round
                   (* 1000.0 (/ (- (current-jiffy) started-at) (jiffies-per-second)))
                 ) ;round
               ) ;inexact->exact
             ) ;duration-ms
             (tr (or (json->pi-tool-result raw-res) raw-res))
             (is-err? (if (pi-tool-result? tr)
                        (pi-tool-result-is-error tr)
                        (pi-agent-assoc-val 'isError "isError" tr #f)
                      ) ;if
             ) ;is-err?
             (content-blks (%extract-tool-result-content-blocks raw-res))
             (text (pi-extract-tool-result-text raw-res))
             (tool-msg (pi-make-tool-result-msg id name content-blks is-err?))
             (res-payload (list (cons 'id id)
                            (cons 'name name)
                            (cons 'tool name)
                            (cons 'input effective-args)
                            (cons 'result text)
                            (cons 'isError is-err?)
                          ) ;list
             ) ;res-payload
            ) ;
        (when reg-emit
          (reg-emit 'tool_result res-payload)
        ) ;when
        (when (procedure? emit)
          (emit 'tool_execution_end id name raw-res is-err? duration-ms)
          (emit 'message_start tool-msg)
          (emit 'message_end tool-msg)
        ) ;when
        (values tool-msg #f)
      ) ;let*
    ) ;define

    (define* (pi-execute-one-tool-call-with-status call emit timeout (opt-config #f))
      (let-values (((id name args) (%tool-call-fields call)))
        (let* ((raw-reg-emit (pi-cfg-ref opt-config 'emit-extension-event))
               (reg-emit (and (procedure? raw-reg-emit) raw-reg-emit))
               (exec (%tool-executor opt-config))
               (call-payload (list (cons 'id id) (cons 'name name) (cons 'tool name) (cons 'input args))
               ) ;call-payload
               (emit-res (if reg-emit (reg-emit 'tool_call call-payload) '((action
                                                                             . continue)))
               ) ;emit-res
               (action (cdr (or (assq 'action emit-res) '(#f . continue))))
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
          (if (eq? action 'block)
            (%handle-blocked-tool-call id name args emit-res reg-emit emit)
            (%run-tool-call id name args emit-res reg-emit exec timeout opt-config emit)
          ) ;if
        ) ;let*
      ) ;let-values
    ) ;define*

    (define* (pi-execute-one-tool-call call emit timeout (opt-config #f))
      (let-values (((tool-msg term?)
                    (pi-execute-one-tool-call-with-status call emit timeout opt-config)
                   ) ;
                  ) ;
        tool-msg
      ) ;let-values
    ) ;define*

    (define (%emit-skipped-call call emit text)
      (let-values (((id name _args) (%tool-call-fields call)))
        (let ((raw-res (pi-tool-result text #t))
              (tool-msg (pi-make-tool-result-msg id name text #t))
             ) ;
          (when (procedure? emit)
            (emit 'tool_execution_start id name '())
            (emit 'tool_execution_end id name raw-res #t)
            (emit 'message_start tool-msg)
            (emit 'message_end tool-msg)
          ) ;when
          tool-msg
        ) ;let
      ) ;let-values
    ) ;define

    (define* (pi-execute-tool-calls-with-status tool-calls emit timeout (opt-config #f))
      (let loop
        ((rest tool-calls) (acc '()) (batch-term? #f))
        (if (null? rest)
          (values (reverse acc) batch-term?)
          (if batch-term?
            (let ((msg (%emit-skipped-call (car rest)
                         emit
                         "Tool call skipped: previous tool call terminated the run"
                       ) ;%emit-skipped-call
                  ) ;msg
                 ) ;
              (loop (cdr rest) (cons msg acc) #t)
            ) ;let
            (if (pi-aborted?)
              (let ((msg (%emit-skipped-call (car rest) emit "Tool call aborted by user")))
                (loop (cdr rest) (cons msg acc) #f)
              ) ;let
              (let-values (((msg term?)
                            (pi-execute-one-tool-call-with-status (car rest) emit timeout opt-config)
                           ) ;
                          ) ;
                (loop (cdr rest) (cons msg acc) term?)
              ) ;let-values
            ) ;if
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
          (let-values (((id name _args) (%tool-call-fields call)))
            (let ((tool-msg (pi-make-tool-result-msg id name
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
            ) ;let
          ) ;let-values
        ) ;lambda
        tool-calls
      ) ;map
    ) ;define

    (define (pi-filter-tool-calls content)
      (let ((lst (cond ((vector? content) (vector->list content))
                       ((list? content) content)
                       (else '())
                 ) ;cond
            ) ;lst
           ) ;
        (filter (lambda (c) (pi-tool-call-block? (%normalize-one-block c))) lst)
      ) ;let
    ) ;define

    (define (pi-extract-tool-result-blocks msgs count)
      (list->vector
        (map
          (lambda (raw)
            (let ((m (or (json->pi-message raw) raw)))
              (if (pi-tool-result-message? m)
                (pi-tool-result-message (pi-tool-result-message-tool-call-id m)
                  (pi-tool-result-message-tool-name m)
                  (%to-block-list (pi-tool-result-message-content m))
                  (pi-tool-result-message-is-error m)
                ) ;pi-tool-result-message
                (pi-tool-result-message (pi-agent-assoc-val 'toolCallId "toolCallId" m "")
                  (pi-agent-assoc-val 'toolName "toolName" m "")
                  (%to-block-list (pi-agent-assoc-val 'content "content" m '()))
                  (pi-agent-assoc-val 'isError "isError" m #f)
                ) ;pi-tool-result-message
              ) ;if
            ) ;let
          ) ;lambda
          (list-tail msgs (- (length msgs) count))
        ) ;map
      ) ;list->vector
    ) ;define

  ) ;begin
) ;define-library
