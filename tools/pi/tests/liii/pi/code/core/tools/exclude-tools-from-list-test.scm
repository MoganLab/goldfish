(import (liii check)
  (liii pi code core tools)
) ;import

(check-set-mode! 'report-failed)

(check (exclude-tools-from-list '("read" "bash" "edit" "write") #f)
  => '("read" "bash" "edit" "write"))
(check (exclude-tools-from-list '("read" "bash" "edit" "write") "")
  => '("read" "bash" "edit" "write"))
(check (exclude-tools-from-list '("read" "bash" "edit" "write") "bash,write,edit")
  => '("read"))
(check (exclude-tools-from-list '("read" "bash" "edit" "write") " bash , edit ")
  => '("read" "write"))
(check (exclude-tools-from-list '("read" "bash" "edit" "write") "foo,bash")
  => '("read" "edit" "write"))
(check (exclude-tools-from-list '("read" "bash") "read,bash") => '())

(check-report)
