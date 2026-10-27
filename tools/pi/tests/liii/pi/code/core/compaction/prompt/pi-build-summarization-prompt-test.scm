(import (liii check) (liii string) (liii pi code core compaction prompt))

(check-set-mode! 'report-failed)

(let ((p (pi-build-summarization-prompt "[User]: hi" #f)))
  (check-true (string-contains? p "<conversation>\n[User]: hi\n</conversation>"))
  (check-false (string-contains? p "<previous-summary>"))
  (check-true (string-contains? p "## Goal"))
) ;let

(let ((p (pi-build-summarization-prompt "new" "old summary")))
  (check-true (string-contains? p "<previous-summary>\nold summary\n</previous-summary>")
  ) ;check-true
) ;let

(check-report)
