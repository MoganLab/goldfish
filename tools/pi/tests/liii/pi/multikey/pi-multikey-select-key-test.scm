(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-multikey-select-key
;; 从密钥池中选取当前可用 key：过滤冷却中的 key 后取最久未用（LRU）的一个。
;; 全部 key 冷却中（pool 耗尽）时返回 #f。

(define fixture-dir
  (if (file-exists? "tests/fixtures/multikey")
    "tests/fixtures/multikey"
    "tools/pi/tests/fixtures/multikey"
  ) ;if
) ;define

;; 1. 初始状态：所有 key last-used 相同，取第一个（alpha）
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (sel (pi-multikey-select-key pool 1000000))
      ) ;
  (check-true (pi-pool-key? sel))
  (check (pi-pool-key-key sel) => "sk-pool-alpha-0001")
) ;let*

;; 2. 选中会更新 last-used（LRU）：下一次选中换到另一个 key
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (sel1 (pi-multikey-select-key pool 1000000))
       (sel2 (pi-multikey-select-key pool 1000001))
       (sel3 (pi-multikey-select-key pool 1000002))
      ) ;
  (check (pi-pool-key-key sel1) => "sk-pool-alpha-0001")
  (check (pi-pool-key-key sel2) => "sk-pool-beta-00002")
  ;; 两个 key 轮换后回到 alpha
  (check (pi-pool-key-key sel3) => "sk-pool-alpha-0001")
) ;let*

;; 3. 冷却中的 key 被过滤：alpha 冷却中，选中 beta
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (alpha (car (pi-key-pool-keys pool)))
       (now 2000000)
      ) ;
  (pi-multikey-mark-cooldown! alpha 429 now pool)
  (let ((sel (pi-multikey-select-key pool now)))
    (check (pi-pool-key-key sel) => "sk-pool-beta-00002")
  ) ;let
) ;let*

;; 4. pool 耗尽：所有 key 冷却中，返回 #f
(let* ((pool (pi-load-multikey fixture-dir "google"))
       (keys (pi-key-pool-keys pool))
       (now 3000000)
      ) ;
  (pi-multikey-mark-cooldown! (car keys) 429 now pool)
  (pi-multikey-mark-cooldown! (cadr keys) 403 now pool)
  (check (pi-multikey-select-key pool now) => #f)
) ;let*

(check-report)
