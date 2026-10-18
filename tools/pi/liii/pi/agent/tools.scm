;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii pi agent tools)
  (import (scheme base)
    (liii list)
    (liii pi event)
    (liii pi tools)
    (liii pi message)
    (liii pi tools types)
    (liii pi tools record)
  ) ;import
  (export pi-agent-assoc-val pi-first-block-from-content
    pi-extract-tool-result-text pi-make-tool-result-msg pi-execute-one-tool-call
    pi-execute-tool-calls pi-fail-tool-calls-length pi-filter-tool-calls
    pi-extract-tool-result-blocks
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

    (define (pi-execute-one-tool-call call emit timeout)
      (let* ((id (pi-agent-assoc-val 'id "id" call ""))
             (name (pi-agent-assoc-val 'name "name" call ""))
             (args (pi-agent-assoc-val 'arguments "arguments" call '()))
            ) ;
        (when (procedure? emit)
          (emit 'tool_execution_start id name args)
        ) ;when
        (let* ((raw-res (execute-tool-call name args :timeout timeout))
               (is-err? (if (pi-tool-result? raw-res)
                          (pi-tool-result-is-error raw-res)
                          (pi-agent-assoc-val 'isError "isError" raw-res #f)
                        ) ;if
               ) ;is-err?
               (text (pi-extract-tool-result-text raw-res))
               (tool-msg (pi-make-tool-result-msg id name text is-err?))
              ) ;
          (when (procedure? emit)
            (emit 'tool_execution_end id name raw-res is-err?)
            (emit 'message_start tool-msg)
            (emit 'message_end tool-msg)
          ) ;when
          tool-msg
        ) ;let*
      ) ;let*
    ) ;define

    (define (pi-execute-tool-calls tool-calls emit timeout)
      (map (lambda (call) (pi-execute-one-tool-call call emit timeout)) tool-calls)
    ) ;define

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
