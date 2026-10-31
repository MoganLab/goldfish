(import (liii check) (liii pi ai providers registry))

(check-set-mode! 'report-failed)

(check (pi-provider-record "unknown-provider") => #f)
(check (pair? (pi-provider-record "google")) => #t)

(check-report)
