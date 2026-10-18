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

(define-library (liii pi message record)
  (import (scheme base) (liii base) (liii pi time) (liii pi message types))
  (export pi-message? pi-message-role pi-message->alist pi-block? pi-block-type
    pi-block->alist pi-user-message pi-user-message? pi-user-message-role
    pi-user-message-content pi-user-message-timestamp pi-assistant-message
    pi-assistant-message? pi-assistant-message-role pi-assistant-message-content
    pi-assistant-message-stop-reason pi-assistant-message-error-message
    pi-assistant-message-usage pi-assistant-message-timestamp
    pi-tool-result-message pi-tool-result-message? pi-tool-result-message-role
    pi-tool-result-message-tool-call-id pi-tool-result-message-tool-name
    pi-tool-result-message-content pi-tool-result-message-is-error
    pi-tool-result-message-timestamp pi-text-block pi-text-block?
    pi-text-block-type pi-text-block-text pi-tool-call-block pi-tool-call-block?
    pi-tool-call-block-type pi-tool-call-block-id pi-tool-call-block-name
    pi-tool-call-block-arguments pi-tool-call-block-thought-signature
  ) ;export
  (begin

    (define (pi-user-message-role msg)
      "user"
    ) ;define
    (define (pi-assistant-message-role msg)
      "assistant"
    ) ;define
    (define (pi-tool-result-message-role msg)
      "toolResult"
    ) ;define

    (define (pi-message? x)
      (or (pi-user-message? x) (pi-assistant-message? x) (pi-tool-result-message? x))
    ) ;define

    (define (pi-message-role msg)
      (cond ((pi-user-message? msg) (pi-user-message-role msg))
            ((pi-assistant-message? msg) (pi-assistant-message-role msg))
            ((pi-tool-result-message? msg) (pi-tool-result-message-role msg))
            ((pair? msg)
             (let ((p (or (assoc "role" msg) (assoc 'role msg))))
               (if p (cdr p) #f)
             ) ;let
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (pi-text-block-type blk)
      "text"
    ) ;define
    (define (pi-tool-call-block-type blk)
      "toolCall"
    ) ;define

    (define (pi-block? x)
      (or (pi-text-block? x) (pi-tool-call-block? x))
    ) ;define

    (define (pi-block-type blk)
      (cond ((pi-text-block? blk) (pi-text-block-type blk))
            ((pi-tool-call-block? blk) (pi-tool-call-block-type blk))
            ((pair? blk)
             (let ((p (or (assoc "type" blk) (assoc 'type blk))))
               (if p (cdr p) #f)
             ) ;let
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (pi-block->alist blk)
      (cond ((pair? blk) blk)
            ((pi-text-block? blk)
             (list (cons "type" "text") (cons "text" (pi-text-block-text blk)))
            ) ;
            ((pi-tool-call-block? blk)
             (let* ((sig (pi-tool-call-block-thought-signature blk))
                    (base (list (cons "type" "toolCall")
                            (cons "id" (pi-tool-call-block-id blk))
                            (cons "name" (pi-tool-call-block-name blk))
                            (cons "arguments" (pi-tool-call-block-arguments blk))
                          ) ;list
                    ) ;base
                   ) ;
               (if sig (append base (list (cons "thoughtSignature" sig))) base)
             ) ;let*
            ) ;
            (else '())
      ) ;cond
    ) ;define

    (define (%normalize-content-for-alist c)
      (cond ((pi-block? c) (pi-block->alist c))
            ((vector? c) (list->vector (map %normalize-content-for-alist (vector->list c))))
            ((pair? c)
             (if (pi-block? (car c))
               (map %normalize-content-for-alist c)
               (map (lambda (x) (if (pi-block? x) (%normalize-content-for-alist x) x)) c)
             ) ;if
            ) ;
            (else c)
      ) ;cond
    ) ;define

    (define (pi-message->alist msg)
      (cond ((pair? msg) msg)
            ((pi-user-message? msg)
             (let ((c (pi-user-message-content msg)) (ts (pi-user-message-timestamp msg)))
               (if ts
                 (list (cons "role" "user") (cons "content" c) (cons "timestamp" ts))
                 (list (cons "role" "user") (cons "content" c))
               ) ;if
             ) ;let
            ) ;
            ((pi-assistant-message? msg)
             (let* ((c (pi-assistant-message-content msg))
                    (sr (pi-assistant-message-stop-reason msg))
                    (err (pi-assistant-message-error-message msg))
                    (u (pi-assistant-message-usage msg))
                    (ts (pi-assistant-message-timestamp msg))
                    (res (list (cons "role" "assistant")
                           (cons "content" c)
                           (cons "stopReason" sr)
                           (cons "errorMessage" err)
                         ) ;list
                    ) ;res
                    (res
                      (if u (append res (list (cons "usage" u))) res)
                    ) ;res
                    (res
                      (if ts (append res (list (cons "timestamp" ts))) res)
                    ) ;res
                   ) ;
               res
             ) ;let*
            ) ;
            ((pi-tool-result-message? msg)
             (let* ((id (pi-tool-result-message-tool-call-id msg))
                    (name (pi-tool-result-message-tool-name msg))
                    (c (pi-tool-result-message-content msg))
                    (is-err (pi-tool-result-message-is-error msg))
                    (ts (pi-tool-result-message-timestamp msg))
                    (res (list (cons "role" "toolResult")
                           (cons "toolCallId" id)
                           (cons "toolName" name)
                           (cons "content" c)
                           (cons "isError" (if is-err #t #f))
                         ) ;list
                    ) ;res
                    (res
                      (if ts (append res (list (cons "timestamp" ts))) res)
                    ) ;res
                   ) ;
               res
             ) ;let*
            ) ;
            (else '())
      ) ;cond
    ) ;define

    (define* (pi-user-message content (timestamp #f))
      (%make-pi-user-message content (or timestamp (pi-current-time-millis)))
    ) ;define*

    (define* (pi-assistant-message content
               (stop-reason "stop")
               (error-message #f)
               (usage #f)
               (timestamp #f)
             ) ;pi-assistant-message
      (%make-pi-assistant-message content
        stop-reason
        error-message
        usage
        (or timestamp (pi-current-time-millis))
      ) ;%make-pi-assistant-message
    ) ;define*

    (define* (pi-tool-result-message tool-call-id
               tool-name
               content
               (is-error #f)
               (timestamp #f)
             ) ;pi-tool-result-message
      (%make-pi-tool-result-message tool-call-id
        tool-name
        content
        (if is-error #t #f)
        (or timestamp (pi-current-time-millis))
      ) ;%make-pi-tool-result-message
    ) ;define*

    (define (pi-text-block text)
      (%make-pi-text-block text)
    ) ;define

    (define* (pi-tool-call-block id name arguments (thought-signature #f))
      (%make-pi-tool-call-block id name arguments thought-signature)
    ) ;define*

  ) ;begin
) ;define-library
