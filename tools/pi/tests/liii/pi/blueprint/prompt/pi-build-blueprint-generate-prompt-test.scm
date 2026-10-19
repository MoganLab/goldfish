(import (liii check)
        (liii string)
        (liii pi blueprint prompt)
) ;import

(check-set-mode! 'report-failed)

;; pi-build-blueprint-generate-prompt
(let ((p (pi-build-blueprint-generate-prompt "Add OAuth2 login")))
  (check-true (string-contains? p "## Objective\nAdd OAuth2 login"))
  (check-true (string-contains? p "2-6 phases"))
  (check-true (string-contains? p "2-8 concrete, agent-sized tasks"))
  (check-true (string-contains? p "ONE JSON object"))
  (check-true (string-contains? p "\"phases\""))
  (check-true (string-contains? p "Do NOT use \"tests_pass\""))
  (check-true (string-contains? p "custom_command"))
  (check-true (string-contains? p "user_approval"))
  (check-true (string-contains? p "\"phase.task\""))
) ;let

(check-report)
