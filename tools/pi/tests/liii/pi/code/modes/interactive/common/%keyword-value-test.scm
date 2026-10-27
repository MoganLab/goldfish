(import (scheme base)
  (liii check)
  (liii pi code modes interactive common))

(check-set-mode! 'report)

(check (%keyword-value :key '(:key "val") #f) => "val")
(check (%keyword-value :missing '(:key "val") "default") => "default")
(check (%keyword-value :key '() "default") => "default")

(check-report)
