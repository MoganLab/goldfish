(import (scheme base)
  (liii check)
  (liii pi code modes interactive footer))

(check-set-mode! 'report)

(check (format-cwd-for-footer "/home/da" "/home/da") => "~")
(check (format-cwd-for-footer "/home/da/git/goldfish" "/home/da") => "~/git/goldfish")
(check (format-cwd-for-footer "/tmp/test" "/home/da") => "/tmp/test")
(check (format-cwd-for-footer "/home/da" #f) => "/home/da")

(check-report)
