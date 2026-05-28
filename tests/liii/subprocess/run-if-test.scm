(import (liii check) (liii os) (liii subprocess))

(when (os-linux?)
  ;; Condition true
  (check (run-if "true" "echo yes") => 0)

  ;; Condition false, no else
  (check (run-if "false" "echo yes") => 1)

  ;; Condition false, with else
  (check (run-if "false" "echo yes" "echo no") => 0)
) ;when

(check-report)
