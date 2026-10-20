(import (liii check)
        (liii pi compact))

(check-set-mode! 'report-failed)

;; pi-compaction-default-settings
;; 与 pi SETTINGS_DEFAULTS.compaction 一致
(let ((s (pi-compaction-default-settings)))
  (check (cdr (assoc 'enabled s)) => #t)
  (check (cdr (assoc 'reserve-tokens s)) => 16384)
  (check (cdr (assoc 'keep-recent-tokens s)) => 20000)
) ;let

;; pi-should-compact?
;; contextWindow - reserveTokens 为阈值
(let ((s '((enabled . #t) (reserve-tokens . 20))))
  (check (pi-should-compact? 81 100 s) => #t)
  (check (pi-should-compact? 80 100 s) => #f)
) ;let

;; enabled=false 不触发
(let ((s '((enabled . #f) (reserve-tokens . 20))))
  (check (pi-should-compact? 1000 100 s) => #f)
) ;let

;; 缺省节使用默认值
(check (pi-should-compact? 16383 100000 '()) => #f)
(check (pi-should-compact? 90000 100000 '()) => #t)

(check-report)
