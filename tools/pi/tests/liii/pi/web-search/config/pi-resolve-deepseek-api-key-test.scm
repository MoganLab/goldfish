;; pi-resolve-deepseek-api-key
;; 解析 deepseek 联网搜索使用的 API Key。
;;
;; 语法
;; ----
;; (pi-resolve-deepseek-api-key config opt-key . opt-env)
;;
;; 参数
;; ----
;; config : list?
;; 会话配置 alist。
;;
;; opt-key : (or string? boolean?)
;; 显式传入的 API Key。
;;
;; opt-env : (or list? boolean?)（可选）
;; 环境变量 alist（测试可控）。
;;
;; 返回值
;; ----
;; (or string? #f)
;; 生效的 API Key 字符串，未配置时返回 #f。

(import (liii check) (liii pi web-search config))

(check-set-mode! 'report-failed)

;; 解析 deepseek 联网搜索使用的 API Key。
(check (pi-resolve-deepseek-api-key '() "opt-key") => "opt-key")
(check (pi-resolve-deepseek-api-key '() "  ") => #f)
(check (pi-resolve-deepseek-api-key '((provider . "deepseek")
                                      (api-key . "cfg-key"))
         #f
       ) ;pi-resolve-deepseek-api-key
  =>
  "cfg-key"
) ;check
;; provider 非 deepseek 时不能用 config 的 api-key（那是别家的 key）
(check (pi-resolve-deepseek-api-key '((provider . "google") (api-key . "g-key"))
         #f
         '(("HOME" . "/nonexistent-gf-test")
           ("PI_CODING_AGENT_DIR" . "/nonexistent-gf-test"))
       ) ;pi-resolve-deepseek-api-key
  =>
  #f
) ;check
;; 环境变量
(check (pi-resolve-deepseek-api-key '()
         #f
         '(("DEEPSEEK_API_KEY" . "env-key")
           ("HOME" . "/nonexistent-gf-test")
           ("PI_CODING_AGENT_DIR" . "/nonexistent-gf-test"))
       ) ;pi-resolve-deepseek-api-key
  =>
  "env-key"
) ;check

(check-report)
