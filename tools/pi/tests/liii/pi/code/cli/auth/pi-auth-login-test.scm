;; pi-auth-login
;; 测试 CLI auth login 子命令与凭证持久化。
;;
;; 语法
;; ----
;; (pi-auth-login provider key)
;; (pi-auth-login provider key config-dir)
;;
;; 参数
;; ----
;; provider : string?
;; 服务商标识名称（如 "deepseek"、"google" 等）。
;;
;; key : string?
;; 服务商 API Key。
;;
;; config-dir : any?
;; 可选，配置目录路径（默认为 (gf-get-config-dir)）。
;;
;; 返回值
;; ----
;; integer?
;; 退出码：成功写入返回 0，失败返回 1。

(import (liii check)
  (liii path)
  (liii string)
  (liii tempfile)
  (liii pi ai auth)
  (liii pi code cli auth)
) ;import

(check-set-mode! 'report-failed)

;; 1. 正确解析并执行 auth login，写入 auth.json
(let* ((tmp (make-temp-dir "gf-auth-cli-" ""))
       (status (run-pi-auth '("login" "--provider" "deepseek" "--key" "sk-cli-123") tmp))
       (creds (pi-load-auth tmp))
      ) ;
  (check status => 0)
  (check (length creds) => 1)
  (check (pi-resolve-provider-key "deepseek" creds '()) => "sk-cli-123")

  ;; 2. 支持位置参数格式：gf pi auth login <provider> <key>
  (let ((status2 (run-pi-auth '("login" "kimi-coding" "sk-kimi-pos") tmp)))
    (check status2 => 0)
    (let ((creds2 (pi-load-auth tmp)))
      (check (length creds2) => 2)
      (check (pi-resolve-provider-key "deepseek" creds2 '()) => "sk-cli-123")
      (check (pi-resolve-provider-key "kimi-coding" creds2 '()) => "sk-kimi-pos")
    ) ;let
  ) ;let

  (path-unlink (path (path-join tmp "auth.json")))
  (path-rmdir tmp)
) ;let*

;; 3. 缺失参数时报错并返回 1
(let* ((tmp (make-temp-dir "gf-auth-cli-" ""))
       (status (run-pi-auth '("login" "--provider" "deepseek") tmp))
      ) ;
  (check status => 1)
  (path-rmdir tmp)
) ;let*

(check-report)
