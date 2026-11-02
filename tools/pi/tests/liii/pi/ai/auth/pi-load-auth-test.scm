;; pi-load-auth
;; 从指定配置目录下的 auth.json 文件加载各服务商凭据列表。
;;
;; 语法
;; ----
;; (pi-load-auth config-dir)
;; (pi-load-auth config-dir opt-env)
;;
;; 参数
;; ----
;; config-dir : string?
;; 配置目录路径（其中应包含 auth.json）。
;;
;; opt-env : list?
;; 可选，自定义环境变量列表，用于插值 key 中的环境变量引用。
;;
;; 返回值
;; ----
;; list?
;; 凭据对象列表；若文件不存在或无效则返回空列表。

(import (liii check)
  (liii path)
  (liii tempfile)
  (liii pi ai config record)
  (liii pi ai auth)
) ;import

(check-set-mode! 'report-failed)

;; 1. 目录不存在或缺少 auth.json 时返回空列表
(let ((tmp (make-temp-dir "gf-auth-test-" "")))
  (check (pi-load-auth tmp) => '())
  (path-rmdir tmp)
) ;let

;; 2. 正常读取 auth.json 并解析凭据
(let* ((tmp (make-temp-dir "gf-auth-test-" ""))
       (auth-file (path-join tmp "auth.json"))
      ) ;
  (path-write-text auth-file
    "{\"deepseek\": {\"type\": \"api_key\", \"key\": \"sk-test-123\"}}"
  ) ;path-write-text
  (let ((creds (pi-load-auth tmp)))
    (check (length creds) => 1)
    (check (pi-credential-provider (car creds)) => "deepseek")
    (check (pi-credential-key (car creds)) => "sk-test-123")
    (check (pi-credential-type (car creds)) => "api_key")
  ) ;let
  (path-unlink auth-file)
  (path-rmdir tmp)
) ;let*

(check-report)
