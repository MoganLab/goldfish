(import (scheme base)
  (liii check)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; get-sessions-root：返回 agent 目录下的会话根目录（<agent-dir>/sessions）

(check (get-sessions-root "/tmp/agent") => "/tmp/agent/sessions")

(check-report)
