(import (liii check)
        (liii pi config) (liii pi session)
        (liii pi compact))

(check-set-mode! 'report-failed)

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/compact-settings")
    "tests/fixtures/compact-settings"
    "tools/pi/tests/fixtures/compact-settings"
  ) ;if
) ;define

;; 读取 settings.json 的 compaction 节
(let ((s (pi-load-compaction-settings FIXTURE_DIR)))
  (check (cdr (assoc 'enabled s)) => #f)
  (check (cdr (assoc 'reserve-tokens s)) => 8000)
  (check (cdr (assoc 'keep-recent-tokens s)) => 9000)
) ;let

;; 无 settings.json 时回退默认值
(let ((s (pi-load-compaction-settings "non/existent/compact-dir")))
  (check (cdr (assoc 'enabled s)) => #t)
  (check (cdr (assoc 'reserve-tokens s)) => 16384)
  (check (cdr (assoc 'keep-recent-tokens s)) => 20000)
) ;let

;; 缺字段时逐项回退
(let ((s (pi-load-compaction-settings "tools/pi/tests/fixtures/default")))
  (check (cdr (assoc 'enabled s)) => #t)
  (check (cdr (assoc 'reserve-tokens s)) => 16384)
) ;let

(check-report)
