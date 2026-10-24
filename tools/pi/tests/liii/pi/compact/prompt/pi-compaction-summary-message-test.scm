(import (liii check)
        (liii string)
        (liii pi ai message) (liii pi compact prompt)
        (liii pi compact))

(check-set-mode! 'report-failed)

;; 前后缀逐字对齐 pi messages.ts
(check PI_COMPACTION_SUMMARY_PREFIX
  => (string-append "The conversation history before this point was compacted into the following summary:\n\n<summary>\n"
     ) ;string-append
) ;check
(check PI_COMPACTION_SUMMARY_SUFFIX => "\n</summary>")

;; pi-compaction-summary-message
(let* ((msg (pi-compaction-summary-message "要点一"))
       (content (pi-user-message-content msg))
      ) ;
  (check (pi-message-role msg) => "user")
  (check (string-starts? content PI_COMPACTION_SUMMARY_PREFIX) => #t)
  (check (string-ends? content PI_COMPACTION_SUMMARY_SUFFIX) => #t)
  (check-true (string-contains? content "要点一"))
) ;let*

;; pi-build-summarization-prompt
(let ((p (pi-build-summarization-prompt "[User]: hi" #f)))
  (check-true (string-contains? p "<conversation>\n[User]: hi\n</conversation>"))
  (check-false (string-contains? p "<previous-summary>"))
  (check-true (string-contains? p "## Goal"))
) ;let

(let ((p (pi-build-summarization-prompt "new" "old summary")))
  (check-true (string-contains? p "<previous-summary>\nold summary\n</previous-summary>"))
) ;let

(check-report)
