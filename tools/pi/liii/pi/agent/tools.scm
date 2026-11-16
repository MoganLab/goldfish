;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi agent tools)
  (import (scheme base)
    (scheme time)
    (liii json)
    (liii string)
    (liii list)
    (liii pi ai abort)
    (liii pi ai config runtime)
    (liii pi ai message)
    (liii pi ai tools definition)
    (liii pi ai tools result)
    (liii pi agent tools-compat)
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
      (let* ((c (cond ((pi-tool-result? res) (pi-tool-result-content res))
                      ((pi-tool-result-message? res) (pi-tool-result-message-content res))
                      ((pair? res) (pi-agent-assoc-val 'content "content" res #()))
                      (else #())
                ) ;cond
             ) ;c
             (first-blk (pi-first-block-from-content c))
             (blk (%maybe-json-block first-blk))
            ) ;
        (if (pi-text-block? blk)
          (pi-text-block-text blk)
          (pi-agent-assoc-val 'text "text" first-blk "")
        ) ;if
      ) ;let*
    ) ;define

    (define (%extract-tool-result-content-blocks res)
      (%to-block-list (cond ((pi-tool-result? res) (pi-tool-result-content res))
                            ((pair? res) (pi-agent-assoc-val 'content "content" res #()))
                            (else res)
                      ) ;cond
      ) ;%to-block-list
    ) ;define

    (define (pi-make-tool-result-msg id name content is-err)
      (pi-tool-result-message id
        name
        (if
          (and (pair? content)
            (not (pi-block? (car content)))
            (not (pair? (car content)))
          ) ;and
          (list (%maybe-json-block content))
          (%to-block-list content)
        ) ;if
        is-err
      ) ;pi-tool-result-message
    ) ;define

    ;; 从 config 取工具执行器（由上层注入；缺失时返回占位错误结果）
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
      (let ((m (%maybe-json-msg msg)))
        (string-join
          (%collect-text-blocks (cond ((pi-assistant-message? m) (pi-assistant-message-content m))
                                      ((pi-user-message? m) (pi-user-message-content m))
                                      ((pi-tool-result-message? m) (pi-tool-result-message-content m))
                                      (else (pi-agent-assoc-val 'content "content" m '()))
                                ) ;cond
          ) ;%collect-text-blocks
          "\n"
        ) ;string-join
      ) ;let
    ) ;define

    (define (%tool-call-fields call)
      (let ((c (%maybe-json-block call)))
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
             (term?
               (if (cdr (or (assq 'terminate emit-res) '(#f . #f))) #t #f)
             ) ;term?
             (tool-msg (pi-make-tool-result-msg id name reason #t))
            ) ;
        (when reg-emit
          (reg-emit 'tool_result
            (list (cons 'id id)
              (cons 'name name)
              (cons 'tool name)
              (cons 'input args)
              (cons 'result reason)
              (cons 'isError #t)
            ) ;list
          ) ;reg-emit
        ) ;when
        (when (procedure? emit)
          (emit 'tool_execution_end
            id
            name
            (if term?
              (list
                (cons 'content (vector (list (cons 'type "text") (cons 'text reason))))
                (cons 'isError #t)
                (cons 'terminate #t)
              ) ;list
              (pi-tool-result reason #t)
            ) ;if
            #t
          ) ;emit
          (emit 'message_start tool-msg)
          (emit 'message_end tool-msg)
        ) ;when
        (values tool-msg term?)
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
             (exec-opts (append (list :timeout timeout)
                          (if (equal? name "bash")
                            (list :on-update on-update :check-abort pi-aborted?)
                            '()
                          ) ;if
                          (if (pair? opt-config) (list :config opt-config) '())
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
             (is-err? (if (pi-tool-result? raw-res)
                        (pi-tool-result-is-error raw-res)
                        (pi-agent-assoc-val 'isError "isError" raw-res #f)
                      ) ;if
             ) ;is-err?
             (tool-msg (pi-make-tool-result-msg id
                         name
                         (%extract-tool-result-content-blocks raw-res)
                         is-err?
                       ) ;pi-make-tool-result-msg
             ) ;tool-msg
            ) ;
        (when reg-emit
          (reg-emit 'tool_result
            (list (cons 'id id)
              (cons 'name name)
              (cons 'tool name)
              (cons 'input effective-args)
              (cons 'result (pi-extract-tool-result-text raw-res))
              (cons 'isError is-err?)
            ) ;list
          ) ;reg-emit
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
        (let* ((reg-emit
                 (let ((r (pi-cfg-ref opt-config 'emit-extension-event)))
                   (and (procedure? r) r)
                 ) ;let
               ) ;reg-emit
               (emit-res
                 (if reg-emit
                   (reg-emit 'tool_call
                     (list (cons 'id id) (cons 'name name) (cons 'tool name) (cons 'input args))
                   ) ;reg-emit
                   '((action . continue))
                 ) ;if
               ) ;emit-res
               (action (cdr (or (assq 'action emit-res) '(#f . continue))))
              ) ;
          (when (procedure? emit)
            (emit 'tool_execution_start id name args)
          ) ;when
          (when (and (not (procedure? emit)) (equal? name "web_search"))
            (let ((q (pi-agent-assoc-val 'query "query" args ""))
                  (p (if (defined? '*stderr*) *stderr* (current-error-port)))
                 ) ;
              (when
                (and (string? q) (not (string-null? (string-trim-both q))))
                (display (string-append "Searching web: " q "...") p)
                (newline p)
              ) ;when
            ) ;let
          ) ;when
          (if (eq? action 'block)
            (%handle-blocked-tool-call id name args emit-res reg-emit emit)
            (%run-tool-call id
              name
              args
              emit-res
              reg-emit
              (%tool-executor opt-config)
              timeout
              opt-config
              emit
            ) ;%run-tool-call
          ) ;if
        ) ;let*
      ) ;let-values
    ) ;define*

    (define* (pi-execute-one-tool-call call emit timeout (opt-config #f))
      (let-values (((msg _) (pi-execute-one-tool-call-with-status call emit timeout opt-config)))
        msg
      ) ;let-values
    ) ;define*

    (define (%emit-skipped-call call emit text)
      (let-values (((id name _args) (%tool-call-fields call)))
        (let ((tool-msg (pi-make-tool-result-msg id name text #t)))
          (when (procedure? emit)
            (emit 'tool_execution_start id name '())
            (emit 'tool_execution_end id name (pi-tool-result text #t) #t)
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
        (cond ((null? rest) (values (reverse acc) batch-term?))
              (batch-term?
                (loop (cdr rest)
                  (cons (%emit-skipped-call (car rest)
                          emit
                          "Tool call skipped: previous tool call terminated the run"
                        ) ;%emit-skipped-call
                    acc
                  ) ;cons
                  #t
                ) ;loop
              ) ;batch-term?
              ((pi-aborted?)
               (loop (cdr rest)
                 (cons (%emit-skipped-call (car rest) emit "Tool call aborted by user") acc)
                 #f
               ) ;loop
              ) ;
              (else
                (let-values (((msg term?)
                              (pi-execute-one-tool-call-with-status (car rest) emit timeout opt-config)
                             ) ;
                            ) ;
                  (loop (cdr rest) (cons msg acc) term?)
                ) ;let-values
              ) ;else
        ) ;cond
      ) ;let
    ) ;define*

    (define* (pi-execute-tool-calls tool-calls emit timeout (opt-config #f))
      (let-values (((res _) (pi-execute-tool-calls-with-status tool-calls emit timeout opt-config))
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
      (filter (lambda (c) (pi-tool-call-block? (%maybe-json-block c)))
        (cond ((vector? content) (vector->list content))
              ((list? content) content)
              (else '())
        ) ;cond
      ) ;filter
    ) ;define

    (define (pi-extract-tool-result-blocks msgs count)
      (list->vector
        (map
          (lambda (raw)
            (let ((m (%maybe-json-msg raw)) (r? pi-tool-result-message?))
              (pi-tool-result-message (if (r? m)
                                        (pi-tool-result-message-tool-call-id m)
                                        (pi-agent-assoc-val 'toolCallId "toolCallId" m "")
                                      ) ;if
                (if (r? m)
                  (pi-tool-result-message-tool-name m)
                  (pi-agent-assoc-val 'toolName "toolName" m "")
                ) ;if
                (%to-block-list (if (r? m)
                                  (pi-tool-result-message-content m)
                                  (pi-agent-assoc-val 'content "content" m '())
                                ) ;if
                ) ;%to-block-list
                (if (r? m)
                  (pi-tool-result-message-is-error m)
                  (pi-agent-assoc-val 'isError "isError" m #f)
                ) ;if
              ) ;pi-tool-result-message
            ) ;let
          ) ;lambda
          (list-tail msgs (- (length msgs) count))
        ) ;map
      ) ;list->vector
    ) ;define

  ) ;begin
) ;define-library
