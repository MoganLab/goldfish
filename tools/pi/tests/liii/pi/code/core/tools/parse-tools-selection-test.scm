(import (liii check)
  (liii pi code core tools)
) ;import

(check-set-mode! 'report-failed)

(check (parse-tools-selection #f) => '("read" "bash" "edit" "write"))
(check (parse-tools-selection "read,grep") => '("read" "grep"))
(check (parse-tools-selection "+grep,-edit") => '("read" "bash" "write" "grep"))
(check (parse-tools-selection "+web_search")
  => '("read" "bash" "edit" "write" "web_search"))
(check (parse-tools-selection "-web_search") => '("read" "bash" "edit" "write"))
(check (parse-tools-selection "web_search") => '("web_search"))
(check (parse-tools-selection "+grep" '("read" "bash" "edit" "write" "web_search"))
  => '("read" "bash" "edit" "write" "web_search" "grep"))
(check (parse-tools-selection #f '("read" "web_search")) => '("read" "web_search"))

(check-report)
