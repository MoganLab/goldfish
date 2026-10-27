(import (liii check) (liii pi code core compaction))

(check-set-mode! 'report-failed)

;; 与 pi SETTINGS_DEFAULTS.compaction 一致
(let ((s (pi-compaction-default-settings)))
  (check (cdr (assoc 'enabled s)) => #t)
  (check (cdr (assoc 'reserve-tokens s)) => 16384)
  (check (cdr (assoc 'keep-recent-tokens s)) => 20000)
) ;let

(check-report)
