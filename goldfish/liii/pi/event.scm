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

(define-library (liii pi event)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii time)
    (liii uuid)
    (liii json)
  ) ;import
  (export pi-iso-8601-utc-now pi-current-time-millis pi-serialize-event
    pi-event-session pi-event-agent-start pi-event-turn-start
    pi-event-message-start pi-event-message-end pi-event-tool-execution-start
    pi-event-tool-execution-end pi-event-turn-end pi-event-agent-end
    pi-event-agent-settled
  ) ;export
  (begin

    (define (%pad-two-digits n)
      (if (< n 10) (string-append "0" (number->string n)) (number->string n))
    ) ;define

    (define (pi-iso-8601-utc-now)
      (let* ((now (current-time))
             (d (time-utc->date now 0))
             (year (number->string (date-year d)))
             (month (%pad-two-digits (date-month d)))
             (day (%pad-two-digits (date-day d)))
             (hour (%pad-two-digits (date-hour d)))
             (minute (%pad-two-digits (date-minute d)))
             (second (%pad-two-digits (date-second d)))
             (millis (%pad-three-digits (quotient (time-nanosecond now) 1000000)))
            ) ;
        (string-append year "-" month "-" day "T" hour ":" minute ":" second "."
          millis "Z"
        ) ;string-append
      ) ;let*
    ) ;define

    (define (%pad-three-digits n)
      (cond ((< n 10) (string-append "00" (number->string n)))
            ((< n 100) (string-append "0" (number->string n)))
            (else (number->string n))
      ) ;cond
    ) ;define

    (define (pi-current-time-millis)
      (let ((now (current-time)))
        (+ (* (time-second now) 1000) (quotient (time-nanosecond now) 1000000))
      ) ;let
    ) ;define

    (define (pi-serialize-event alist)
      (json->string alist)
    ) ;define

    (define* (pi-event-session cwd (id #f) (timestamp #f))
      (list (cons "type" "session")
        (cons "version" 3)
        (cons "id" (or id (uuid4)))
        (cons "timestamp" (or timestamp (pi-iso-8601-utc-now)))
        (cons "cwd" cwd)
      ) ;list
    ) ;define*

    (define (pi-event-agent-start)
      '(("type" . "agent_start"))
    ) ;define

    (define (pi-event-turn-start)
      '(("type" . "turn_start"))
    ) ;define

    (define (pi-event-message-start msg)
      (list (cons "type" "message_start") (cons "message" msg))
    ) ;define

    (define (pi-event-message-end msg)
      (list (cons "type" "message_end") (cons "message" msg))
    ) ;define

    (define (pi-event-tool-execution-start tool-call-id tool-name args)
      (list (cons "type" "tool_execution_start")
        (cons "toolCallId" tool-call-id)
        (cons "toolName" tool-name)
        (cons "args" args)
      ) ;list
    ) ;define

    (define (pi-event-tool-execution-end tool-call-id tool-name result is-error)
      (list (cons "type" "tool_execution_end")
        (cons "toolCallId" tool-call-id)
        (cons "toolName" tool-name)
        (cons "result" result)
        (cons "isError" (if is-error #t #f))
      ) ;list
    ) ;define

    (define* (pi-event-turn-end assistant-msg (tool-results #()))
      (list (cons "type" "turn_end")
        (cons "message" assistant-msg)
        (cons "toolResults" tool-results)
      ) ;list
    ) ;define*

    (define* (pi-event-agent-end messages (will-retry #f))
      (list (cons "type" "agent_end")
        (cons "messages" messages)
        (cons "willRetry" (if will-retry #t #f))
      ) ;list
    ) ;define*

    (define* (pi-event-agent-settled (aborted #f))
      (list (cons "type" "agent_settled") (cons "aborted" (if aborted #t #f)))
    ) ;define*

  ) ;begin
) ;define-library
