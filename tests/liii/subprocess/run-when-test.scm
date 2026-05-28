(import (liii check) (liii os) (liii subprocess))

(when (os-linux?)
  ;; Condition false (non-zero), execute
  (check (run-when "false" "echo yes") => 0)

  ;; Condition true (zero), don't execute
  (check (run-when "true" "echo yes") => 0)
) ;when

(check-report)
