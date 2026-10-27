(import (liii check)
  (liii os)
  (liii string)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; get-session-dir 与 make-session-file-path 的路径组合集成
(let* ((agent-dir "/tmp/test-agent")
       (cwd "/home/da/git/goldfish")
       (sdir (get-session-dir agent-dir cwd))
       (file-path (make-session-file-path sdir "2026-10-17T09:29:29.706Z" "test-id-123")))
  (check (if (os-windows?) (string-replace sdir "\\" "/") sdir)
    => "/tmp/test-agent/sessions/--home-da-git-goldfish--")
  (check (if (os-windows?) (string-replace file-path "\\" "/") file-path)
    => "/tmp/test-agent/sessions/--home-da-git-goldfish--/2026-10-17T09-29-29-706Z_test-id-123.jsonl"))

(check-report)
