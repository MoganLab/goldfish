;; Introduced private definitions must belong to the consuming library.
(import (scheme base) (liii check)
        (prefix (r7rs-audit macro-location-a) a-)
        (prefix (r7rs-audit macro-location-b) b-))
(check-set-mode! 'report-failed)
(check (list a-counter (a-read-counter) b-counter (b-read-counter))
       => '(10 10 100 100))
(check-report)
