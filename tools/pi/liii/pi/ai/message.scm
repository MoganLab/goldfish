;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai message)
  (import (scheme base)
    (liii base)
    (liii pi ai message types)
    (liii pi ai message record)
  ) ;import
  (export pi-message? pi-message-role pi-message->alist pi-block? pi-block-type
    pi-block->alist pi-user-message pi-user-message? pi-user-message-role
    pi-user-message-content pi-user-message-timestamp
    pi-bash-execution->user-text pi-assistant-message pi-assistant-message?
    pi-assistant-message-role pi-assistant-message-content
    pi-assistant-message-stop-reason pi-assistant-message-error-message
    pi-assistant-message-usage pi-assistant-message-timestamp
    pi-tool-result-message pi-tool-result-message? pi-tool-result-message-role
    pi-tool-result-message-tool-call-id pi-tool-result-message-tool-name
    pi-tool-result-message-content pi-tool-result-message-is-error
    pi-tool-result-message-timestamp pi-text-block pi-text-block?
    pi-text-block-type pi-text-block-text pi-thinking-block pi-thinking-block?
    pi-thinking-block-type pi-thinking-block-thinking
    pi-thinking-block-thinking-signature pi-tool-call-block pi-tool-call-block?
    pi-tool-call-block-type pi-tool-call-block-id pi-tool-call-block-name
    pi-tool-call-block-arguments pi-tool-call-block-thought-signature
  ) ;export
  (begin
  ) ;begin
) ;define-library
