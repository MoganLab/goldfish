(import (liii check)
  (liii pi ai message)
  (liii pi code core compaction tokens)
  (liii pi code core compaction)
) ;import

(check-set-mode! 'report-failed)

(check (pi-estimate-content-chars "abcdefgh") => 8)
(check (pi-estimate-content-chars (list (pi-text-block "ab") (pi-text-block "cde")))
  =>
  5
) ;check
(check (pi-estimate-content-chars "") => 0)

(check-report)
