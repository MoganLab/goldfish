(import (liii check) (liii pi multikey))

(check-set-mode! 'report-failed)

;; pi-multikey-select-key
;; 从密钥池中选取当前可用 key：优先继续使用最近使用的可用 key。
;; 首次从配置顺序的第一个 key 开始；当前 key 冷却后才切换；全部冷却时返回 #f。

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

;; 2. 正常连续选择固定使用 key-1，不做 LRU 轮询
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (sel1 (pi-multikey-select-key pool 1000000))
       (sel2 (pi-multikey-select-key pool 1000001))
       (sel3 (pi-multikey-select-key pool 1000002))
       (sel4 (pi-multikey-select-key pool 1000003))
      ) ;
  (check (pi-pool-key-key sel1) => "sk-pool-alpha-0001")
  (check (pi-pool-key-key sel2) => "sk-pool-alpha-0001")
  (check (pi-pool-key-key sel3) => "sk-pool-alpha-0001")
  (check (pi-pool-key-key sel4) => "sk-pool-alpha-0001")
) ;let*

;; 3. 当前 key 冷却后才切换；后续持续使用新 key
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (alpha (car (pi-key-pool-keys pool)))
       (now 2000000)
      ) ;
  (pi-multikey-select-key pool now)
  (pi-multikey-mark-cooldown! alpha 429 now pool)
  (let ((sel1 (pi-multikey-select-key pool now))
        (sel2 (pi-multikey-select-key pool (+ now 1))))
    (check (pi-pool-key-key sel1) => "sk-pool-beta-00002")
    (check (pi-pool-key-key sel2) => "sk-pool-beta-00002")
  ) ;let
) ;let*

;; 4. 旧 key 冷却结束不抢回：当前 key-2 正常时继续使用 key-2
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (alpha (car (pi-key-pool-keys pool)))
       (now 3000000)
      ) ;
  (pi-multikey-select-key pool now)
  (pi-multikey-mark-cooldown! alpha 429 now pool)
  (pi-multikey-select-key pool (+ now 1))
  (let ((sel (pi-multikey-select-key pool (+ now 30001))))
    (check (pi-pool-key-key sel) => "sk-pool-beta-00002")
  ) ;let
) ;let*

;; 5. pool 耗尽：所有 key 冷却中，返回 #f
(let* ((pool (pi-load-multikey fixture-dir "google"))
       (keys (pi-key-pool-keys pool))
       (now 3000000)
      ) ;
  (pi-multikey-mark-cooldown! (car keys) 429 now pool)
  (pi-multikey-mark-cooldown! (cadr keys) 403 now pool)
  (check (pi-multikey-select-key pool now) => #f)
) ;let*

(check-report)
