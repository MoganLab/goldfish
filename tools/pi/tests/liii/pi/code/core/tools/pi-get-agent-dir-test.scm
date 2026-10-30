(import (scheme process-context)
        (liii check)
        (liii string)
        (liii os)
        (liii path)
        (liii pi code config) (liii pi code core tools read))

(check-set-mode! 'report-failed)

;; pi-get-agent-dir
;; 获取 agent 配置基准目录路径（path 对象）。

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/default")
      "tests/fixtures/default"
      "tools/pi/tests/fixtures/default"))

(define (%normalize dir)
  (if (os-windows?) (string-replace (path->string dir) "\\" "/") (path->string dir))
) ;define

;; 1. 显式目录参数优先
(check-true (path? (pi-get-agent-dir)))
(let ((dir (pi-get-agent-dir FIXTURE_DIR)))
  (check-true (string-ends? (%normalize dir) FIXTURE_DIR)))

;; 2. 真实进程环境中的 PI_CODING_AGENT_DIR 生效（生产语义）
(let ((saved (get-environment-variable "PI_CODING_AGENT_DIR")))
  (putenv "PI_CODING_AGENT_DIR" "/tmp/pi-agent-dir-from-env")
  (check (pi-get-agent-dir) => (pi-abs-path "/tmp/pi-agent-dir-from-env"))
  (if saved
    (putenv "PI_CODING_AGENT_DIR" saved)
    (unsetenv "PI_CODING_AGENT_DIR")
  ) ;if
) ;let

;; 3. 污染环境下显式目录仍然胜出（防回归：不得被真实环境覆盖）
(let ((saved (get-environment-variable "PI_CODING_AGENT_DIR")))
  (putenv "PI_CODING_AGENT_DIR" "/tmp/pi-agent-dir-from-env")
  (let ((dir (pi-get-agent-dir FIXTURE_DIR)))
    (check-true (string-ends? (%normalize dir) FIXTURE_DIR)))
  (if saved
    (putenv "PI_CODING_AGENT_DIR" saved)
    (unsetenv "PI_CODING_AGENT_DIR")
  ) ;if
) ;let

;; 4. 真实环境无 PI_CODING_AGENT_DIR 时回退到 ~/.pi/agent（HOME 隔离，与机器无关）
(let ((saved-agent (get-environment-variable "PI_CODING_AGENT_DIR"))
      (saved-home (get-environment-variable "HOME"))
      (saved-profile (get-environment-variable "USERPROFILE")))
  (unsetenv "PI_CODING_AGENT_DIR")
  (putenv "HOME" "/tmp/pi-home-isolated")
  (when (os-windows?) (putenv "USERPROFILE" "C:/tmp/pi-home-isolated"))
  (let ((dir (pi-get-agent-dir)))
    (check-true (string-ends? (%normalize dir)
                              "pi-home-isolated/.pi/agent")))
  (if saved-agent
    (putenv "PI_CODING_AGENT_DIR" saved-agent)
    (unsetenv "PI_CODING_AGENT_DIR")
  ) ;if
  (if saved-home
    (putenv "HOME" saved-home)
    (unsetenv "HOME")
  ) ;if
  (when (os-windows?)
    (if saved-profile
      (putenv "USERPROFILE" saved-profile)
      (unsetenv "USERPROFILE")
    ) ;if
  ) ;when
) ;let

(check-report)
