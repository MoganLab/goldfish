(import (liii check) (liii goldcode) (liii path) (liii string) (scheme file))

(check-set-mode! 'report-failed)

;; backup-and-remove-settings!
;; gf code 启动 claude 前处理 ~/.claude/settings.json：
;; 先备份到 settings.json.default（已有备份不覆盖），然后删除 settings 文件。
;; settings.json.default 正是 default profile 的加载文件，这样备份文件
;; 和默认 profile 合二为一。因为 claude --settings 是合并加载，删除默认
;; settings 可避免其中未被 profile 文件覆盖的键残留生效。
;;
;; 语法
;; ----
;; (backup-and-remove-settings! settings backup)
;;
;; 参数
;; ----
;; settings : string? 将被删除的 settings 文件路径
;; backup : string? 备份文件路径
;;
;; 说明
;; ----
;; - settings 存在且 backup 不存在时：复制 settings 为 backup，然后删除 settings
;; - backup 已存在时不覆盖（避免把后来的内容当成原始配置备份），仍删除 settings
;; - settings 不存在时不产生 backup，什么也不做

(define tmp-dir (path->string (path-temp-dir)))

(define settings (string-append tmp-dir "/gf-test-settings.json"))

(define backup (string-append tmp-dir "/gf-test-settings.json.default"))

(define (cleanup!)
  (when (file-exists? settings)
    (delete-file settings)
  ) ;when
  (when (file-exists? backup)
    (delete-file backup)
  ) ;when
) ;define

;; settings 存在且无 backup：生成内容相同的 backup，settings 被删除
(cleanup!)
(path-write-text (path settings) "original")
(backup-and-remove-settings! settings backup)
(check (file-exists? backup) => #t)
(check (path-read-text (path backup)) => "original")
(check (file-exists? settings) => #f)

;; settings 存在且 backup 已存在：不覆盖已有备份，settings 被删除
(path-write-text (path settings) "profile")
(backup-and-remove-settings! settings backup)
(check (path-read-text (path backup)) => "original")
(check (file-exists? settings) => #f)

;; settings 不存在：不产生 backup
(cleanup!)
(backup-and-remove-settings! settings backup)
(check (file-exists? backup) => #f)

(cleanup!)

(check-report)
