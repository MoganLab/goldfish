;; pi-save-auth-key
;; 在指定或默认配置目录下保存或增量更新 auth.json 中的服务商 API 密钥凭证。
;;
;; 语法
;; ----
;; (pi-save-auth-key provider key)
;; (pi-save-auth-key provider key opt-config-dir)
;;
;; 参数
;; ----
;; provider : (or string? symbol?)
;; 服务商标识（如 "kimi-coding"、'deepseek 等）。
;;
;; key : string?
;; 服务商 API Key（非空字符串）。
;;
;; opt-config-dir : (or string? path?)
;; 可选，配置目录路径（默认为 ~/.gf/agent）。若目录不存在会自动递归创建。
;;
;; 返回值
;; ----
;; string?
;; 写入的 auth.json 绝对文件路径。

(import (liii check)
  (liii path)
  (liii tempfile)
  (liii pi ai config record)
  (liii pi ai auth)
) ;import

(check-set-mode! 'report-failed)

;; 1. 参数校验
(check-catch 'type-error (pi-save-auth-key 123 "key"))
(check-catch 'type-error (pi-save-auth-key "deepseek" 123))
(check-catch 'value-error (pi-save-auth-key "   " "key"))
(check-catch 'value-error (pi-save-auth-key "deepseek" "   "))

;; 2. 新建 auth.json 并保存凭据
(let* ((tmp (make-temp-dir "gf-auth-save-" ""))
       (target (pi-save-auth-key "deepseek" "sk-test-ds-1" tmp))
       (creds (pi-load-auth tmp))
      ) ;
  (check-true (string? target))
  (check-true (path-exists? (path target)))
  (check (length creds) => 1)
  (check (pi-credential-provider (car creds)) => "deepseek")
  (check (pi-credential-key (car creds)) => "sk-test-ds-1")
  (path-unlink (path target))
  (path-rmdir tmp)
) ;let*

;; 3. 增量追加第二个服务商，不破坏旧凭证
(let* ((tmp (make-temp-dir "gf-auth-save-" ""))
       (target1 (pi-save-auth-key 'deepseek "sk-ds-1" tmp))
       (target2 (pi-save-auth-key "kimi-coding" "sk-kimi-2" tmp))
       (creds (pi-load-auth tmp))
      ) ;
  (check (length creds) => 2)
  (check (pi-resolve-provider-key "deepseek" creds '()) => "sk-ds-1")
  (check (pi-resolve-provider-key "kimi-coding" creds '()) => "sk-kimi-2")

  ;; 4. 覆盖更新已有服务商凭证
  (pi-save-auth-key "deepseek" "sk-ds-updated" tmp)
  (let ((updated-creds (pi-load-auth tmp)))
    (check (length updated-creds) => 2)
    (check (pi-resolve-provider-key "deepseek" updated-creds '()) => "sk-ds-updated")
    (check (pi-resolve-provider-key "kimi-coding" updated-creds '()) => "sk-kimi-2")
  ) ;let

  (path-unlink (path (path-join tmp "auth.json")))
  (path-rmdir tmp)
) ;let*

;; 5. 自动递归创建不存在的配置目录
(let* ((tmp (make-temp-dir "gf-auth-save-nested-" ""))
       (nested-dir (path-join tmp "deep" "config" "dir"))
       (target (pi-save-auth-key "google" "g-key-test" nested-dir))
       (creds (pi-load-auth nested-dir))
      ) ;
  (check (length creds) => 1)
  (check (pi-credential-provider (car creds)) => "google")
  (check (pi-credential-key (car creds)) => "g-key-test")
  (path-unlink (path target))
  (path-rmdir (path-join tmp "deep" "config" "dir"))
  (path-rmdir (path-join tmp "deep" "config"))
  (path-rmdir (path-join tmp "deep"))
  (path-rmdir tmp)
) ;let*

(check-report)
