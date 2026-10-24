(import (liii check) (liii pi ai config record) (liii pi ai multikey))

(check-set-mode! 'report-failed)

;; pi-multikey-mark-cooldown!
;; 请求遇 HTTP 429/403（或 key 失效）时冷却对应 key：
;;   429 → cooldownMs（默认 20000，pool 可配置）
;;   403 / "invalid" → invalidKeyCooldownMs（默认 600000，pool 可配置）
;; pi-pool-key-cooling? 判断指定时刻 key 是否仍在冷却期。

(define fixture-dir
  (if (file-exists? "tests/fixtures/multikey")
    "tests/fixtures/multikey"
    "tools/pi/tests/fixtures/multikey"
  ) ;if
) ;define

;; 1. 429 → 使用 pool 配置的 cooldownMs=30000
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (k (car (pi-key-pool-keys pool)))
       (now 1000000)
      ) ;
  (pi-multikey-mark-cooldown! k 429 now pool)
  (check-true (pi-pool-key-cooling? k now))
  ;; 冷却期内（now+29999 < now+30000）
  (check-true (pi-pool-key-cooling? k (+ now 29999)))
  ;; 冷却期结束
  (check-true (not (pi-pool-key-cooling? k (+ now 30000))))
) ;let*

;; 2. 403 → 使用 pool 配置的 invalidKeyCooldownMs=300000
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (k (car (pi-key-pool-keys pool)))
       (now 1000000)
      ) ;
  (pi-multikey-mark-cooldown! k 403 now pool)
  (check-true (pi-pool-key-cooling? k (+ now 299999)))
  (check-true (not (pi-pool-key-cooling? k (+ now 300000))))
) ;let*

;; 3. "invalid"（key 失效判定）→ 与 403 同等待遇
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (k (car (pi-key-pool-keys pool)))
       (now 1000000)
      ) ;
  (pi-multikey-mark-cooldown! k "invalid" now pool)
  (check-true (pi-pool-key-cooling? k (+ now 299999)))
  (check-true (not (pi-pool-key-cooling? k (+ now 300000))))
) ;let*

;; 4. 默认冷却时长（google pool 未配置）：429→20000，403→600000
(let* ((pool (pi-load-multikey fixture-dir "google"))
       (k1 (car (pi-key-pool-keys pool)))
       (k2 (cadr (pi-key-pool-keys pool)))
       (now 1000000)
      ) ;
  (pi-multikey-mark-cooldown! k1 429 now pool)
  (pi-multikey-mark-cooldown! k2 403 now pool)
  (check-true (not (pi-pool-key-cooling? k1 (+ now 20000))))
  (check-true (pi-pool-key-cooling? k2 (+ now 599999)))
  (check-true (not (pi-pool-key-cooling? k2 (+ now 600000))))
) ;let*

;; 5. 再次冷却会刷新冷却截止时间
(let* ((pool (pi-load-multikey fixture-dir "google"))
       (k (car (pi-key-pool-keys pool)))
      ) ;
  (pi-multikey-mark-cooldown! k 429 1000000 pool)
  (pi-multikey-mark-cooldown! k 429 5000000 pool)
  (check-true (pi-pool-key-cooling? k 5000000))
  (check-true (not (pi-pool-key-cooling? k (+ 5000000 20000))))
) ;let*

(check-report)
