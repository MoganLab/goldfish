;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compact prompt)
  (import (scheme base) (liii string) (liii pi ai message))
  (export PI_COMPACTION_SUMMARY_PREFIX PI_COMPACTION_SUMMARY_SUFFIX
    PI_SUMMARIZATION_SYSTEM_PROMPT PI_SUMMARIZATION_PROMPT
    PI_UPDATE_SUMMARIZATION_PROMPT pi-compaction-summary-message
    pi-build-summarization-prompt
  ) ;export
  (begin

    ;; 逐字对齐 pi packages/coding-agent/src/core/messages.ts
    (define PI_COMPACTION_SUMMARY_PREFIX
      "The conversation history before this point was compacted into the following summary:\n\n<summary>\n"
    ) ;define

    (define PI_COMPACTION_SUMMARY_SUFFIX "\n</summary>")

    (define PI_SUMMARIZATION_SYSTEM_PROMPT
      (string-append "You are a context summarization assistant. Your task is to read a conversation between a user and an AI assistant, then produce a structured summary following the exact format specified.\n\n"
        "Do NOT continue the conversation. Do NOT respond to any questions in the conversation. ONLY output the structured summary."
      ) ;string-append
    ) ;define

    (define PI_SUMMARIZATION_PROMPT
      (string-append "The messages above are a conversation to summarize. Create a structured context checkpoint summary that another LLM will use to continue the work.\n\n"
        "Use this EXACT format:\n\n" "## Goal\n"
        "[What is the user trying to accomplish? Can be multiple items if the session covers different tasks.]\n\n"
        "## Constraints & Preferences\n"
        "- [Any constraints, preferences, or requirements mentioned by user]\n"
        "- [Or \"(none)\" if none were mentioned]\n\n" "## Progress\n"
        "### Done\n" "- [x] [Completed tasks/changes]\n\n" "### In Progress\n"
        "- [ ] [Current work]\n\n" "### Blocked\n"
        "- [Issues preventing progress, if any]\n\n" "## Key Decisions\n"
        "- **[Decision]**: [Brief rationale]\n\n" "## Next Steps\n"
        "1. [Ordered list of what should happen next]\n\n"
        "## Critical Context\n"
        "- [Any data, examples, or references needed to continue]\n"
        "- [Or \"(none)\" if not applicable]\n\n"
        "Keep each section concise. Preserve exact file paths, function names, and error messages."
      ) ;string-append
    ) ;define

    (define PI_UPDATE_SUMMARIZATION_PROMPT
      (string-append "The messages above are NEW conversation messages to incorporate into the existing summary provided in <previous-summary> tags.\n\n"
        "Update the existing structured summary with new information. RULES:\n"
        "- PRESERVE all existing information from the previous summary\n"
        "- ADD new progress, decisions, and context from the new messages\n"
        "- UPDATE the Progress section: move items from \"In Progress\" to \"Done\" when completed\n"
        "- UPDATE \"Next Steps\" based on what was accomplished\n"
        "- PRESERVE exact file paths, function names, and error messages\n"
        "- If something is no longer relevant, you may remove it\n\n"
        "Use this EXACT format:\n\n" "## Goal\n"
        "[Preserve existing goals, add new ones if the task expanded]\n\n"
        "## Constraints & Preferences\n"
        "- [Preserve existing, add new ones discovered]\n\n" "## Progress\n"
        "### Done\n"
        "- [x] [Include previously done items AND newly completed items]\n\n"
        "### In Progress\n"
        "- [ ] [Current work - update based on progress]\n\n" "### Blocked\n"
        "- [Current blockers - remove if resolved]\n\n" "## Key Decisions\n"
        "- **[Decision]**: [Brief rationale] (preserve all previous, add new)\n\n"
        "## Next Steps\n" "1. [Update based on current state]\n\n"
        "## Critical Context\n"
        "- [Preserve important context, add new if needed]\n\n"
        "Keep each section concise. Preserve exact file paths, function names, and error messages."
      ) ;string-append
    ) ;define

    (define (pi-compaction-summary-message summary)
      (pi-user-message (string-append PI_COMPACTION_SUMMARY_PREFIX
                         summary
                         PI_COMPACTION_SUMMARY_SUFFIX
                       ) ;string-append
      ) ;pi-user-message
    ) ;define

    (define (pi-build-summarization-prompt conversation-text previous-summary)
      (string-append "<conversation>\n"
        conversation-text
        "\n</conversation>\n\n"
        (if previous-summary
          (string-append "<previous-summary>\n"
            previous-summary
            "\n</previous-summary>\n\n"
          ) ;string-append
          ""
        ) ;if
        (if previous-summary PI_UPDATE_SUMMARIZATION_PROMPT PI_SUMMARIZATION_PROMPT)
      ) ;string-append
    ) ;define

  ) ;begin
) ;define-library
