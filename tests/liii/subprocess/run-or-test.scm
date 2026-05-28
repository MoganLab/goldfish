(import (liii check) (liii os) (liii subprocess))

(when (os-linux?)
  ;; First success stops
  (check (run-or "true" "false") => 0)

  ;; All failure returns last code
  (check (run-or "false" "false") => 1)

  ;; Last success
  (check (run-or "false" "true") => 0)

  ;; :cwd shared
  (check (run-or "true" "false" :cwd "/tmp") => 0)
) ;when

(check-report)
