(import (liii check) (liii goldcode) (liii path) (liii string))

(check-set-mode! 'report-failed)

;; profile-settings-path
;; 根据配置名生成对应的 Claude Code settings 文件路径。
;;
;; 语法
;; ----
;; (profile-settings-path profile)
;;
;; 参数
;; ----
;; profile : string?
;; 配置名，例如 "bigmodel"。
;;
;; 返回值
;; ----
;; string?
;; 返回 ~/.claude/settings.json.<profile> 的绝对路径。

(define home-dot-claude (path->string (path-join (path-home) (path ".claude"))))

(check (profile-settings-path "bigmodel")
  =>
  (string-append home-dot-claude "/settings.json.bigmodel")
) ;check
(check (profile-settings-path "deepseek")
  =>
  (string-append home-dot-claude "/settings.json.deepseek")
) ;check

(check-report)
