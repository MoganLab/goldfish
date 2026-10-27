(import (scheme base)
  (liii check)
  (liii os)
  (liii string)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; get-sessions-root：返回 agent 目录下的会话根目录（<agent-dir>/sessions）
;; Windows 下 path->string 用反斜杠拼接，归一化为 / 后再断言

(let ((root (get-sessions-root "/tmp/agent")))
  (check (if (os-windows?) (string-replace root "\\" "/") root)
    =>
    "/tmp/agent/sessions"
  ) ;check
) ;let

(check-report)
