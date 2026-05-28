(import (liii check) (liii os) (liii subprocess))

(when (os-linux?)
  ;; Runs all regardless of failure
  (check (run-sequence "true" "false" "true") => 0)
  (check (run-sequence "false" "true") => 0)
  (check (run-sequence "true" "false") => 1)

  ;; :cwd shared
  (check (run-sequence "true" "true" :cwd "/tmp") => 0)
) ;when

(check-report)
