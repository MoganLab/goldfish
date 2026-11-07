;; pi-load-compaction-settings
;; 从 settings.json 的 compaction 节加载上下文压缩设置。
;;
;; 语法
;; ----
;; (pi-load-compaction-settings config-dir) -> compaction-settings
;;
;; 参数
;; ----
;; config-dir : string
;; JSON 配置目录。
;;
;; 返回值
;; ----
;; compaction-settings
;; 压缩设置 record；文件或字段缺失时回退默认值。

(import (liii check)
  (liii pi code config settings)
  (liii pi code core session-manager)
  (liii pi code core compaction)
) ;import

(check-set-mode! 'report-failed)

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/compact-settings")
    "tests/fixtures/compact-settings"
    "tools/pi/tests/fixtures/compact-settings"
  ) ;if
) ;define

;; 读取 settings.json 的 compaction 节
(let ((s (pi-load-compaction-settings FIXTURE_DIR)))
  (check (pi-compaction-settings-enabled s) => #f)
  (check (pi-compaction-settings-reserve-tokens s) => 8000)
  (check (pi-compaction-settings-keep-recent-tokens s) => 9000)
) ;let

;; 无 settings.json 时回退默认值
(let ((s (pi-load-compaction-settings "non/existent/compact-dir")))
  (check (pi-compaction-settings-enabled s) => #t)
  (check (pi-compaction-settings-reserve-tokens s) => 16384)
  (check (pi-compaction-settings-keep-recent-tokens s) => 20000)
) ;let

;; 缺字段时逐项回退
(let ((s (pi-load-compaction-settings "tools/pi/tests/fixtures/default")))
  (check (pi-compaction-settings-enabled s) => #t)
  (check (pi-compaction-settings-reserve-tokens s) => 16384)
) ;let

(check-report)
