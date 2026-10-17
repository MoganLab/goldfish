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

(define-library (liii goldpi-agent)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii time)
    (liii goldpi-provider)
    (liii goldpi-tools)
  ) ;import
  (export run-agent-loop extract-assistant-text)
  (begin

    (define (%assoc-val key alt-key alist default-val)
      (if (not (pair? alist))
        default-val
        (let ((pair (or (assoc key alist) (assoc alt-key alist))))
          (if pair (cdr pair) default-val)
        ) ;let
      ) ;if
    ) ;define

    (define (%collect-text-blocks blks)
      (let loop
        ((rest blks) (acc '()))
        (if (null? rest)
          (reverse acc)
          (let* ((blk (car rest)) (t-val (%assoc-val 'type "type" blk "")))
            (if (equal? t-val "text")
              (let ((t (%assoc-val 'text "text" blk "")))
                (loop (cdr rest) (cons t acc))
              ) ;let
              (loop (cdr rest) acc)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (extract-assistant-text msg)
      (let* ((content (%assoc-val 'content "content" msg '()))
             (texts (%collect-text-blocks content))
            ) ;
        (string-join texts "\n")
      ) ;let*
    ) ;define

    (define (%first-block-from-content c)
      (cond
       ((and (vector? c) (> (vector-length c) 0)) (vector-ref c 0))
       ((and (pair? c) (pair? (car c))) (car c))
       (else '())
      ) ;cond
    ) ;define

    (define (%extract-tool-result-text res)
      (let* ((c (%assoc-val 'content "content" res #()))
             (first-blk (%first-block-from-content c))
            ) ;
        (%assoc-val 'text "text" first-blk "")
      ) ;let*
    ) ;define

    (define (%make-tool-result-msg id name text is-err)
      (list (cons 'role "toolResult")
        (cons 'toolCallId id)
        (cons 'toolName name)
        (cons 'content (list (list (cons 'type "text") (cons 'text text))))
        (cons 'isError is-err)
        (cons 'timestamp (time-second (current-time)))
      ) ;list
    ) ;define

    (define (%execute-one-tool-call call emit timeout)
      (let* ((id (%assoc-val 'id "id" call ""))
             (name (%assoc-val 'name "name" call ""))
             (args (%assoc-val 'arguments "arguments" call '()))
            ) ;
        (when (procedure? emit)
          (emit 'tool_execution_start id name args)
        ) ;when
        (let* ((raw-res (execute-tool-call name args :timeout timeout))
               (is-err? (%assoc-val 'isError "isError" raw-res #f))
               (text (%extract-tool-result-text raw-res))
               (tool-msg (%make-tool-result-msg id name text is-err?))
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

    (define (%execute-tool-calls tool-calls emit timeout)
      (map (lambda (call) (%execute-one-tool-call call emit timeout)) tool-calls)
    ) ;define

    (define (%fail-tool-calls-length tool-calls emit)
      (map
        (lambda (call)
          (let* ((id (%assoc-val 'id "id" call ""))
                 (name (%assoc-val 'name "name" call ""))
                 (tool-msg (%make-tool-result-msg id name
                             "Tool call skipped: model output truncated by token limit"
                             #t
                           ) ;%make-tool-result-msg
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

    (define (%make-max-turns-error)
      (list (cons 'role "assistant")
        (cons 'content '())
        (cons 'stopReason "error")
        (cons 'errorMessage "Maximum turns limit reached")
        (cons 'usage '((input . 0) (output . 0) (total . 0)))
      ) ;list
    ) ;define

    (define (%filter-tool-calls content)
      (filter (lambda (c) (equal? (%assoc-val 'type "type" c "") "toolCall")) content)
    ) ;define

    (define (%step-agent-turn config msgs system-prompt emit timeout)
      (when (procedure? emit)
        (emit 'turn_start)
      ) ;when
      (let* ((active-tools (cdr (assoc 'tools config)))
             (resp (gemini-request config msgs system-prompt active-tools timeout))
            ) ;
        (when (procedure? emit)
          (emit 'message_start resp)
          (emit 'message_end resp)
        ) ;when
        (let* ((new-msgs (append msgs (list resp)))
               (stop-reason (%assoc-val 'stopReason "stopReason" resp ""))
               (content (%assoc-val 'content "content" resp '()))
               (tool-calls (%filter-tool-calls content))
              ) ;
          (values new-msgs stop-reason tool-calls resp)
        ) ;let*
      ) ;let*
    ) ;define

    (define (%finish-agent-turn new-msgs stop-reason tool-calls emit)
      (if (equal? stop-reason "length")
        (if (null? tool-calls)
          new-msgs
          (append new-msgs (%fail-tool-calls-length tool-calls emit))
        ) ;if
        new-msgs
      ) ;if
    ) ;define

    (define (%notify-turn-end emit assistant-msg tool-results)
      (when (procedure? emit)
        (emit 'turn_end assistant-msg tool-results)
      ) ;when
    ) ;define

    (define (%extract-tool-result-blocks msgs count)
      (let* ((tail (list-tail msgs (- (length msgs) count))))
        (list->vector
          (map
            (lambda (m)
              (let* ((id (%assoc-val 'toolCallId "toolCallId" m ""))
                     (name (%assoc-val 'toolName "toolName" m ""))
                     (c (%assoc-val 'content "content" m '()))
                     (text (%assoc-val 'text "text" (%first-block-from-content c) ""))
                     (is-err? (%assoc-val 'isError "isError" m #f))
                    ) ;
                (list (cons "role" "toolResult")
                  (cons "toolCallId" id)
                  (cons "toolName" name)
                  (cons "content" (vector (list (cons "type" "text") (cons "text" text))))
                  (cons "isError" (if is-err? #t #f))
                ) ;list
              ) ;let*
            ) ;lambda
            tail
          ) ;map
        ) ;list->vector
      ) ;let*
    ) ;define

    (define* (run-agent-loop config
               initial-messages
               system-prompt
               (emit #f)
               (timeout 120)
               (max-turns 200)
             ) ;run-agent-loop
      (when (procedure? emit)
        (emit 'agent_start)
        (for-each (lambda (m) (emit 'message_start m) (emit 'message_end m))
          initial-messages
        ) ;for-each
      ) ;when
      (let loop
        ((msgs initial-messages) (turn 0))
        (if (and (> max-turns 0) (>= turn max-turns))
          (let* ((err-msg (%make-max-turns-error)) (final-msgs (append msgs (list err-msg))))
            (when (procedure? emit)
              (emit 'turn_start)
              (emit 'message_start err-msg)
              (emit 'message_end err-msg)
              (emit 'turn_end err-msg #())
              (emit 'agent_end final-msgs #f)
              (emit 'agent_settled #f)
            ) ;when
            final-msgs
          ) ;let*
          (let*-values (((new-msgs stop-reason tool-calls resp)
                         (%step-agent-turn config msgs system-prompt emit timeout)
                        ) ;
                       ) ;
            (if (equal? stop-reason "toolUse")
              (let* ((tools-res (%execute-tool-calls tool-calls emit timeout))
                     (all-msgs (append new-msgs tools-res))
                     (res-vec (%extract-tool-result-blocks all-msgs (length tools-res)))
                    ) ;
                (%notify-turn-end emit resp res-vec)
                (loop all-msgs (+ turn 1))
              ) ;let*
              (let ((final-msgs (%finish-agent-turn new-msgs stop-reason tool-calls emit)))
                (%notify-turn-end emit resp #())
                (when (procedure? emit)
                  (let ((aborted? (equal? stop-reason "aborted")))
                    (emit 'agent_end final-msgs #f)
                    (emit 'agent_settled aborted?)
                  ) ;let
                ) ;when
                final-msgs
              ) ;let
            ) ;if
          ) ;let*-values
        ) ;if
      ) ;let
    ) ;define*

  ) ;begin
) ;define-library
