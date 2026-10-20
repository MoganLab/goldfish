;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi)
  (import (scheme base)
    (liii string)
    (liii uuid)
    (liii pi truncate)
    (liii pi read)
    (liii pi write)
    (liii pi edit)
    (liii pi bash)
    (liii pi ls)
    (liii pi find)
    (liii pi grep)
    (liii pi web-search)
    (liii pi code-review)
    (liii pi simplify)
    (liii pi red-green)
    (liii pi blueprint)
    (liii pi compass)
    (liii pi tools)
    (liii pi tools types)
    (liii pi tools record)
    (liii pi auth)
    (liii pi multikey)
    (liii pi models)
    (liii pi config)
    (liii pi gemini)
    (liii pi anthropic)
    (liii pi kimi)
    (liii pi deepseek)
    (liii pi minimax)
    (liii pi zai)
    (liii pi prompt)
    (liii pi message)
    (liii pi config types)
    (liii pi config record)
    (liii pi types)
    (liii pi time)
    (liii pi retry)
    (liii pi args)
    (liii pi modes)
    (liii pi session)
    (liii pi agent)
    (liii pi compact)
  ) ;import
  (export resolve-app-mode format-file-block build-initial-prompt
    get-tool-definition execute-tool-call exclude-tools-from-list
    PI_DEFAULT_MAX_LINES PI_DEFAULT_MAX_BYTES PI_GREP_MAX_LINE_LENGTH
    pi-format-size pi-truncate-line pi-truncate-head pi-truncate-tail pi-arg-ref
    pi-resolve-path pi-read-schema pi-execute-read pi-write-schema
    pi-execute-write pi-edit-schema pi-execute-edit pi-bash-schema
    pi-execute-bash pi-find-bash-path pi-ls-schema pi-execute-ls pi-find-schema
    pi-execute-find pi-grep-schema pi-execute-grep pi-web-search-schema
    pi-validate-web-search-args pi-format-web-search-result
    pi-execute-web-search pi-resolve-config-value json-dict-ref pi-get-agent-dir
    PI_REVIEW_MAX_FILES PI_REVIEW_MAX_FILE_CHARS PI_REVIEW_SEVERITIES
    pi-parse-changed-files pi-changed-files-command pi-collect-changed-files
    pi-review-file-records pi-detect-language pi-language-checklist
    pi-build-language-section pi-build-review-prompt pi-build-fallback-prompt
    pi-parse-review-findings pi-format-review-findings pi-build-review-injection
    pi-edited-files-from-tool-calls pi-validate-review-flags
    pi-parse-changed-ranges pi-diff-ranges-command pi-collect-simplify-files
    pi-build-scope-lines pi-format-range pi-build-simplify-prompt
    pi-build-simplify-apply-prompt pi-parse-simplify-suggestions
    pi-format-simplify-suggestions pi-tdd-state? pi-tdd-state-phase
    pi-tdd-state-task pi-tdd-state-test-files pi-tdd-state-impl-files
    pi-tdd-state-last-test-run pi-tdd-state-phase-history
    pi-tdd-state-turn-index pi-tdd-state-warning pi-tdd-initial-state
    pi-tdd-idle-state pi-tdd-set-warning pi-tdd-add-test-file
    pi-tdd-add-impl-file pi-tdd-set-last-test-run pi-tdd-set-turn-index
    pi-tdd-stale-test-run? pi-tdd-advance-phase pi-tdd-current-state
    pi-tdd-set-current-state! pi-tdd-active? pi-tdd-start! pi-tdd-bump-turn!
    pi-tdd-reset! pi-tdd-glob-match? pi-tdd-language-of pi-classify-tdd-file
    pi-tdd-test-run-command? pi-tdd-parse-test-run-result
    pi-tdd-test-run-trigger pi-tdd-exit-code-from-text pi-tdd-write-gate
    pi-tdd-note-edited-file! pi-tdd-observe-tool! pi-tdd-red-phase-prompt
    pi-tdd-green-phase-prompt pi-tdd-refactor-phase-prompt
    pi-build-red-green-prompt pi-tdd-turn-system-prompt pi-format-tdd-summary
    pi-tdd-status-schema pi-tdd-advance-schema pi-tdd-reset-schema
    pi-execute-tdd-status pi-execute-tdd-advance pi-execute-tdd-reset
    pi-tdd-status-definition pi-tdd-advance-definition pi-tdd-reset-definition
    run-red-green-flow pi-tdd-extend-tool-names pi-blueprint-task
    pi-blueprint-phase pi-blueprint-gate pi-blueprint-blueprint
    pi-blueprint-new-id pi-task-id pi-task-done? pi-phase-id pi-phase-tasks
    pi-phase-gates pi-gate-type pi-gate-description pi-gate-command
    pi-gate-passed? pi-blueprint-objective pi-blueprint-status
    pi-blueprint-phases pi-blueprint-active-phase-id pi-blueprint-active-task-id
    pi-blueprint-all-tasks pi-blueprint-detect-cycles pi-blueprint-get-next-task
    pi-blueprint-start-task pi-blueprint-complete-task pi-blueprint-skip-task
    pi-blueprint-advance-phase pi-blueprint-create pi-blueprint-abandon
    pi-parse-blueprint-plan pi-validate-blueprint-plan pi-render-blueprint-plan
    pi-format-blueprint-progress pi-build-blueprint-context
    pi-build-blueprint-generate-prompt pi-run-blueprint-gate pi-blueprint-load!
    pi-blueprint-save-current! pi-execute-blueprint-update
    pi-execute-blueprint-status pi-execute-blueprint-next run-blueprint-flow
    pi-blueprint-extend-tool-names pi-resolve-turn-system-prompt
    pi-compass-detect-project pi-compass-build-directory-tree
    pi-compass-format-directory-tree pi-compass-detect-packages
    pi-compass-detect-frameworks pi-compass-detect-entry-points
    pi-compass-detect-build-scripts pi-compass-detect-conventions
    pi-compass-detect-key-files pi-compass-content-hash
    pi-compass-generate-codemap pi-compass-render-codemap
    pi-compass-truncate-codemap pi-compass-detect-topics pi-compass-build-tour
    pi-compass-render-tour pi-compass-describe-file pi-compass-load-codemap
    pi-compass-save-codemap! pi-compass-get-or-generate
    pi-compass-build-injection pi-compass-injection-text pi-execute-codebase-map
    pi-execute-code-tour pi-compass-extend-tool-names pi-compass-validate
    pi-compass-local-flow? pi-compass-flag? run-compass-flow
    pi-auto-review-system-prompt pi-read-json-file pi-load-settings
    pi-resolve-proxy-url pi-load-auth pi-resolve-provider-key pi-load-multikey
    pi-multikey-select-key pi-multikey-mark-cooldown! pi-multikey-pool-of
    pi-multikey-request-key pi-multikey-rotate! pi-mask-key pi-pool-key
    pi-pool-key? pi-pool-key-key pi-pool-key-label pi-pool-key-enabled
    pi-pool-key-last-used pi-pool-key-cooling? pi-key-pool pi-key-pool?
    pi-key-pool-id pi-key-pool-cooldown-ms pi-key-pool-invalid-cooldown-ms
    pi-key-pool-keys pi-load-models-store pi-load-models-json
    pi-merge-models-json-into-store pi-resolve-model-metadata
    pi-format-token-count pi-list-models pi-format-models-table
    PI_SUPPORTED_PROVIDERS PI_PROVIDER_BUILTIN_MODELS pi-provider-default-model
    pi-find-model-provider pi-provider-has-model? pi-route-provider-and-model
    pi-ensure-catalog-meta pi-builtin-augment-store pi-builtin-augment-all
    pi-build-models-catalog pi-catalog-provider-names pi-catalog-provider-models
    pi-catalog-merge-provider-models pi-fuzzy-match-model?
    pi-split-search-tokens gemini-map-finish-reason gemini-map-usage
    gemini-normalize-tool-call-id gemini-generate-tool-call-id
    gemini-build-contents gemini-build-request-body
    gemini-build-tool-declarations gemini-build-search-tools
    gemini-inject-google-search gemini-extract-grounding-details
    gemini-grounding-search-queries gemini-grounding-search-results
    gemini-grounding-citations anthropic-map-stop-reason anthropic-map-usage
    anthropic-build-tool-declarations anthropic-build-messages
    anthropic-build-request-body anthropic-parse-response
    anthropic-is-retriable-error? anthropic-normalize-input
    anthropic-parse-tool-arguments kimi-map-stop-reason kimi-map-usage
    kimi-build-tool-declarations kimi-build-messages kimi-build-request-body
    kimi-parse-response kimi-is-retriable-error? make-kimi-stream-decoder
    kimi-build-endpoint kimi-build-tool-declarations-from-names kimi-request
    kimi-request-stream deepseek-map-stop-reason deepseek-map-usage
    deepseek-build-tool-declarations deepseek-build-messages
    deepseek-build-request-body deepseek-parse-response
    deepseek-is-retriable-error? deepseek-build-tool-declarations-from-names
    deepseek-request deepseek-request-stream minimax-map-stop-reason
    minimax-map-usage minimax-build-tool-declarations minimax-build-messages
    minimax-build-request-body minimax-parse-response
    minimax-is-retriable-error? minimax-build-tool-declarations-from-names
    minimax-request minimax-request-stream
    zai-map-stop-reason zai-map-usage zai-build-tool-declarations
    zai-build-messages zai-build-request-body zai-parse-response
    zai-is-retriable-error? zai-build-tool-declarations-from-names
    zai-request zai-request-stream zai-build-endpoint zai-build-headers
    zai-build-proxy-alist pi-resolve-prompt-input
    pi-load-project-context pi-build-system-prompt PI_BUILTIN_TOOL_SNIPPETS
    PI_BUILTIN_TOOL_GUIDELINES pi-iso-8601-utc-now pi-current-time-millis
    pi-current-time-seconds pi-current-monotonic-millis
    pi-current-monotonic-seconds pi-serialize-event assoc pi-event?
    pi-event-type pi-event->alist pi-event-session pi-event-session?
    pi-event-session-type pi-event-session-version pi-event-session-id
    pi-event-session-timestamp pi-event-session-cwd pi-event-agent-start
    pi-event-agent-start? pi-event-agent-start-type pi-event-turn-start
    pi-event-turn-start? pi-event-turn-start-type pi-event-message-start
    pi-event-message-start? pi-event-message-start-type
    pi-event-message-start-message pi-event-message-update
    pi-event-message-update? pi-event-message-update-type
    pi-event-message-update-usage
    pi-event-message-update-assistant-message-event pi-event-message-end
    pi-event-message-end? pi-event-message-end-type pi-event-message-end-message
    pi-event-tool-execution-start pi-event-tool-execution-start?
    pi-event-tool-execution-start-type
    pi-event-tool-execution-start-tool-call-id
    pi-event-tool-execution-start-tool-name pi-event-tool-execution-start-args
    pi-event-tool-execution-update pi-event-tool-execution-update?
    pi-event-tool-execution-update-type
    pi-event-tool-execution-update-tool-call-id
    pi-event-tool-execution-update-tool-name pi-event-tool-execution-update-args
    pi-event-tool-execution-update-partial-result pi-event-tool-execution-end
    pi-event-tool-execution-end? pi-event-tool-execution-end-type
    pi-event-tool-execution-end-tool-call-id
    pi-event-tool-execution-end-tool-name pi-event-tool-execution-end-result
    pi-event-tool-execution-end-is-error pi-event-turn-end pi-event-turn-end?
    pi-event-turn-end-type pi-event-turn-end-message
    pi-event-turn-end-tool-results pi-event-agent-end pi-event-agent-end?
    pi-event-agent-end-type pi-event-agent-end-messages
    pi-event-agent-end-will-retry pi-event-agent-end-cost
    pi-event-agent-end-savings pi-event-agent-settled pi-event-agent-settled?
    pi-event-agent-settled-type pi-event-agent-settled-aborted
    pi-build-json-event uuid4 uuid7 pi-message? pi-message-role
    pi-message->alist pi-block? pi-block-type pi-block->alist pi-user-message
    pi-user-message? pi-user-message-role pi-user-message-content
    pi-user-message-timestamp pi-assistant-message pi-assistant-message?
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
    pi-retry-config pi-retry-config? pi-retry-config-enabled
    pi-retry-config-max-retries pi-retry-config-base-delay-ms
    pi-retry-config-max-agent-delay-ms pi-retry-config-provider-max-retries
    pi-retry-config->alist pi-resolve-retry-params pi-extract-retry-after
    pi-parse-retry-after pi-calc-retry-delay pi-log-retry pi-settings
    pi-settings? pi-settings-default-provider pi-settings-default-model
    pi-settings-default-thinking-level pi-settings-http-proxy pi-settings-retry
    pi-settings-default-tools pi-settings->alist pi-credential pi-credential?
    pi-credential-provider pi-credential-key pi-credential-type
    pi-credential->alist pi-model-metadata pi-model-metadata?
    pi-model-metadata-id pi-model-metadata-name pi-model-metadata-provider
    pi-model-metadata-base-url pi-model-metadata-context-window
    pi-model-metadata-max-tokens pi-model-metadata-matched?
    pi-model-metadata->alist pi-tool-definition pi-tool-definition?
    pi-tool-definition-name pi-tool-definition-description
    pi-tool-definition-parameters pi-tool-definition-execute
    pi-tool-definition-snippet pi-tool-definition-guidelines
    pi-tool-definition->alist pi-tool-result pi-tool-result?
    pi-tool-result-content pi-tool-result-is-error pi-tool-result-isError
    pi-tool-result->alist pi-trunc-result pi-trunc-result?
    pi-trunc-result-content pi-trunc-result-truncated pi-trunc-result-truncated?
    pi-trunc-result-truncated-by pi-trunc-result-total-lines
    pi-trunc-result-total-bytes pi-trunc-result-output-lines
    pi-trunc-result-output-bytes pi-trunc-result-first-line-exceeds-limit?
    pi-trunc-result-first-line-truncated pi-trunc-result-last-line-partial?
    pi-trunc-result-last-line-truncated pi-trunc-result->alist
    pi-load-compaction-settings pi-estimate-content-chars
    pi-estimate-message-tokens pi-context-token-estimate pi-should-compact?
    pi-compaction-default-settings pi-valid-cut-point? pi-find-cut-point
    pi-messages-to-summarize pi-truncate-for-summary pi-serialize-conversation
    pi-extract-file-ops pi-compute-file-lists pi-format-file-operations
    PI_COMPACTION_SUMMARY_PREFIX PI_COMPACTION_SUMMARY_SUFFIX
    PI_SUMMARIZATION_SYSTEM_PROMPT PI_SUMMARIZATION_PROMPT
    PI_UPDATE_SUMMARIZATION_PROMPT pi-compaction-summary-message
    pi-build-summarization-prompt pi-summarization-failure pi-generate-summary
    pi-read-session-entries pi-project-session-messages
    pi-last-compaction-summary pi-append-compaction-entry! pi-compact-session!
  ) ;export
  (begin

    (define (resolve-app-mode mode-option print-flag stdin-tty stdout-tty)
      (cond ((or (equal? mode-option "json") (eq? mode-option 'json)) 'json)
            ((or print-flag (not stdin-tty) (not stdout-tty)) 'print)
            (else 'print)
      ) ;cond
    ) ;define

    (define (build-initial-prompt stdin-content file-text messages)
      (let* ((has-stdin? (and (string? stdin-content) (not (string-null? stdin-content))))
             (has-file? (and (string? file-text) (not (string-null? file-text))))
             (has-msg? (and (pair? messages) (string? (car messages))))
             (first-msg (if has-msg? (car messages) #f))
             (rest-msgs (if has-msg? (cdr messages) '()))
             (parts (append (if has-stdin? (list stdin-content) '())
                      (if has-file? (list file-text) '())
                      (if first-msg (list first-msg) '())
                    ) ;append
             ) ;parts
             (initial-message (if (null? parts) "" (apply string-append parts)))
            ) ;
        (values initial-message rest-msgs)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
