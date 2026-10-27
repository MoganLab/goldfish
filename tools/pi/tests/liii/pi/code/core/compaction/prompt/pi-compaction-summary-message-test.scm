(import (liii check)
  (liii string)
  (liii pi ai message)
  (liii pi code core compaction prompt)
  (liii pi code core compaction)
) ;import

(check-set-mode! 'report-failed)

;; 前后缀逐字对齐 pi messages.ts
(check PI_COMPACTION_SUMMARY_PREFIX
  =>
  "The conversation history before this point was compacted into the following summary:\n\n<summary>\n"
) ;check
(check PI_COMPACTION_SUMMARY_SUFFIX => "\n</summary>")

(let* ((msg (pi-compaction-summary-message "要点一"))
       (content (pi-user-message-content msg))
      ) ;
  (check (pi-message-role msg) => "user")
  (check (string-starts? content PI_COMPACTION_SUMMARY_PREFIX) => #t)
  (check (string-ends? content PI_COMPACTION_SUMMARY_SUFFIX) => #t)
  (check-true (string-contains? content "要点一"))
) ;let*

(check-report)
