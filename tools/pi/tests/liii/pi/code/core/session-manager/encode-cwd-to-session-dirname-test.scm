(import (liii check)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

(check (encode-cwd-to-session-dirname "/home/da/git/goldfish")
  => "--home-da-git-goldfish--")
(check (encode-cwd-to-session-dirname "/var/log") => "--var-log--")

(check-report)
