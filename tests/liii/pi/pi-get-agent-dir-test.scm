(import (liii check) (liii string) (liii pi))

(check-set-mode! 'report-failed)

;; pi-get-agent-dir
;; 获取 agent 配置基准目录路径。
(check-true (string? (pi-get-agent-dir)))
(check-true (string-ends? (pi-get-agent-dir) "/.pi/agent"))
(check-true (string-ends? (pi-get-agent-dir "tools/pi/tests/fixtures/default")
                          "/tools/pi/tests/fixtures/default"))

(check-report)
