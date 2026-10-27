(import (liii check) (liii pi ai config record) (liii pi code config) (liii pi code core session-manager))

(check-set-mode! 'report-failed)

;; pi-load-auth
;; 从 agent 配置目录的 auth.json 加载 API 凭据。
;;
;; 语法
;; ----
;; (pi-load-auth agent-dir . opt-env)
;;
;; 参数
;; ----
;; agent-dir : string
;; 包含 auth.json 的配置目录。
;; opt-env : alist
;; 可选环境变量表，用于解析凭据中的环境变量或命令插值。
;;
;; 返回值
;; ----
;; list
;; pi-credential record 列表；文件不存在或无有效凭据时返回空列表。
;;
;; 说明
;; ----
;; 仅加载 type=api_key 且成功解析 key 的条目。
;; 可通过 assoc 按 provider 查询凭据，无效命令插值会抛出 pi-config-error。
;;
;; 示例
;; ----
;; (pi-credential-key (car (pi-load-auth "~/.pi/agent"))) => "secret"
(define fixture-dir
  (if (file-exists? "tests/fixtures/default")
    "tests/fixtures/default"
    "tools/pi/tests/fixtures/default"
  ) ;if
) ;define

(define invalid-cmd-dir
  (if (file-exists? "tests/fixtures/invalid_cmd")
    "tests/fixtures/invalid_cmd"
    "tools/pi/tests/fixtures/invalid_cmd"
  ) ;if
) ;define

(let ((auth (pi-load-auth fixture-dir
                          '(("TEST_ANTHROPIC_KEY" . "sk-ant-test-12345")))))
  (check-true (pi-credential? (car auth)))
  (check (pi-credential-provider (car auth)) => "google")
  (check (pi-credential-key (car auth)) => "test-google-key-from-auth-json")
  (check (cdr (assoc "google" auth)) => "test-google-key-from-auth-json")
  (check (cdr (assoc "anthropic" auth)) => "sk-ant-test-12345")
  (check (assoc "kimi-coding" auth) => #f))

(check-catch 'pi-config-error
  (pi-load-auth invalid-cmd-dir))

(check-report)
