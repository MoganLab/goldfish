(import (scheme process-context)
        (liii check)
        (liii string)
        (liii os)
        (liii path)
        (liii pi))

(check-set-mode! 'report-failed)

;; pi-get-agent-dir
;; 获取 agent 配置基准目录路径。

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/default")
      "tests/fixtures/default"
      "tools/pi/tests/fixtures/default"))

(define TEMP_TEST_DIR
  (path->string (path-resolve (path-join (path-temp-dir) "pi-agent-dir-from-env"))))

;; 1. 显式目录参数优先
(check-true (string? (pi-get-agent-dir)))
(check-true (string-ends? (pi-get-agent-dir FIXTURE_DIR) FIXTURE_DIR))

;; 2. 真实进程环境中的 PI_CODING_AGENT_DIR 生效（生产语义）
(let ((saved (get-environment-variable "PI_CODING_AGENT_DIR")))
  (putenv "PI_CODING_AGENT_DIR" TEMP_TEST_DIR)
  (check (pi-get-agent-dir) => TEMP_TEST_DIR)
  (if saved
    (putenv "PI_CODING_AGENT_DIR" saved)
    (unsetenv "PI_CODING_AGENT_DIR")
  ) ;if
) ;let

;; 3. 污染环境下显式目录仍然胜出（防回归：不得被真实环境覆盖）
(let ((saved (get-environment-variable "PI_CODING_AGENT_DIR")))
  (putenv "PI_CODING_AGENT_DIR" TEMP_TEST_DIR)
  (check-true (string-ends? (pi-get-agent-dir FIXTURE_DIR) FIXTURE_DIR))
  (if saved
    (putenv "PI_CODING_AGENT_DIR" saved)
    (unsetenv "PI_CODING_AGENT_DIR")
  ) ;if
) ;let

;; 4. 真实环境无 PI_CODING_AGENT_DIR 时回退到 ~/.pi/agent（HOME 隔离，与机器无关）
(let* ((saved-agent (get-environment-variable "PI_CODING_AGENT_DIR"))
       (home-var (if (os-windows?) "USERPROFILE" "HOME"))
       (saved-home (get-environment-variable home-var))
       (isolated-home (path->string (path-resolve (path-join (path-temp-dir) "pi-home-isolated")))))
  (unsetenv "PI_CODING_AGENT_DIR")
  (putenv home-var isolated-home)
  (check-true (string-ends? (path-as-posix (path (pi-get-agent-dir))) "/.pi/agent"))
  (check-true (string-contains? (path-as-posix (path (pi-get-agent-dir)))
                                (path-as-posix (path isolated-home))))
  (if saved-agent
    (putenv "PI_CODING_AGENT_DIR" saved-agent)
    (unsetenv "PI_CODING_AGENT_DIR")
  ) ;if
  (if saved-home
    (putenv home-var saved-home)
    (unsetenv home-var)
  ) ;if
) ;let

(check-report)
