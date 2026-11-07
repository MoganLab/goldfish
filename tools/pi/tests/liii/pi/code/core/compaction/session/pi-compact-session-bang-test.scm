;; pi-compact-session!
;; 对会话文件执行一次压缩流程。
;;
;; 语法
;; ----
;; (pi-compact-session! config file-path settings . opt-timeout opt-reason)
;;
;; 参数
;; ----
;; config : list
;; Agent 配置 alist。
;;
;; file-path : string
;; 会话文件路径。
;;
;; settings : pi-compaction-settings
;; 压缩配置记录。
;;
;; opt-timeout : integer（可选）
;; 摘要生成超时秒数，缺省为 600。
;;
;; opt-reason : string（可选）
;; 压缩触发原因，缺省为 "manual"。
;;
;; 返回值
;; ----
;; alist
;; 包含 id、summary、first-kept-entry-id 等字段的压缩结果。

(import (liii check)
  (liii base)
  (liii path)
  (liii pi code core compaction session)
  (liii pi code core compaction tokens)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证为过程对象
(check-true (procedure? pi-compact-session!))

;; 2. 会话内容过短时抛出 compact-error

(define tmp-file
  (path-join (path-temp-dir) "pi-compact-session-bang-test.jsonl")
) ;define

(when (path-exists? tmp-file)
  (path-unlink tmp-file)
) ;when

(check-catch 'compact-error
  (pi-compact-session! '()
    (path->string tmp-file)
    (make-pi-compaction-settings #t 100 100)
  ) ;pi-compact-session!
) ;check-catch

(when (path-exists? tmp-file)
  (path-unlink tmp-file)
) ;when

(check-report)
