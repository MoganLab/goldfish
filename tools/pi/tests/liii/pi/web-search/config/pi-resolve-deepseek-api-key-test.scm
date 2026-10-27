(import (liii check)
        (liii pi web-search config)
) ;import

(check-set-mode! 'report-failed)

;; pi-resolve-deepseek-api-key
(check (pi-resolve-deepseek-api-key '() "opt-key") => "opt-key")
(check (pi-resolve-deepseek-api-key '() "  ") => #f) ; 空白 opt 视为未设置
(check (pi-resolve-deepseek-api-key '((provider . "deepseek") (api-key . "cfg-key")) #f) => "cfg-key")
;; provider 非 deepseek 时不能用 config 的 api-key（那是别家的 key）
(check (pi-resolve-deepseek-api-key '((provider . "google") (api-key . "g-key")) #f
              '(("HOME" . "/nonexistent-gf-test")
                ("PI_CODING_AGENT_DIR" . "/nonexistent-gf-test"))) => #f)
;; 环境变量
(check (pi-resolve-deepseek-api-key '() #f '(("DEEPSEEK_API_KEY" . "env-key")
                                              ("HOME" . "/nonexistent-gf-test")
                                              ("PI_CODING_AGENT_DIR" . "/nonexistent-gf-test"))) => "env-key")

(check-report)
