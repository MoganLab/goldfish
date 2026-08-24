(import (liii check) (liii goldcode) (liii path) (liii string) (scheme file))

(check-set-mode! 'report-failed)

;; backup-settings!
;; gf code --profile 启动 claude 前备份 settings.json；替换后保留 profile 内容，不做恢复。
;;
;; 语法
;; ----
;; (backup-settings! settings backup)
;;
;; 参数
;; ----
;; settings : string? 将被替换的 settings 文件路径
;; backup : string? 备份文件路径
;;
;; 说明
;; ----
;; - settings 存在且 backup 不存在时，复制 settings 为 backup
;; - backup 已存在时不覆盖（避免把上次替换进来的 profile 内容当成原始配置备份）
;; - settings 不存在时不产生 backup

(define tmp-dir (path->string (path-temp-dir)))

(define settings (string-append tmp-dir "/gf-test-settings.json"))

(define backup (string-append tmp-dir "/gf-test-settings.json.gf-backup"))

(define (cleanup!)
  (when (file-exists? settings)
    (delete-file settings)
  ) ;when
  (when (file-exists? backup)
    (delete-file backup)
  ) ;when
) ;define

;; settings 存在且无 backup：生成内容相同的 backup，settings 不变
(cleanup!)
(path-write-text (path settings) "original")
(backup-settings! settings backup)
(check (file-exists? backup) => #t)
(check (path-read-text (path backup)) => "original")
(check (path-read-text (path settings)) => "original")

;; backup 已存在：不覆盖已有备份（settings 里是上次替换进来的 profile 内容）
(path-write-text (path settings) "profile")
(backup-settings! settings backup)
(check (path-read-text (path backup)) => "original")

;; settings 不存在：不产生 backup
(cleanup!)
(backup-settings! settings backup)
(check (file-exists? backup) => #f)

(cleanup!)

(check-report)
