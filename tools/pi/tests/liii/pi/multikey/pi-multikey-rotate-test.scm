(import (liii check) (liii pi ai time) (liii pi multikey pool) (liii pi multikey load) (liii pi multikey policy))

(check-set-mode! 'report-failed)

;; pi-multikey-rotate!
;; 请求遇 429/403 时冷却当前 key 并切换到下一可用 key：
;;   429 → 按 429 冷却时长；403/invalid → 按 invalid 冷却时长；
;;   仍有可用 key 时返回新 key 字符串；pool 全部冷却时返回 'exhausted；
;;   无 pool 时返回 #f（调用方走既有重试/报错路径）。

(define fixture-dir
  (if (file-exists? "tests/fixtures/multikey")
    "tests/fixtures/multikey"
    "tools/pi/tests/fixtures/multikey"
  ) ;if
) ;define

;; 1. 429 冷却当前 key 并切到下一 key；之后选取跳过冷却中的 key
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (cfg (list (cons 'multikey-pool pool) (cons 'api-key "sk-pool-alpha-0001")))
       (next (pi-multikey-rotate! cfg "sk-pool-alpha-0001" 429))
       (alpha (car (pi-key-pool-keys pool)))
      ) ;
  (check next => "sk-pool-beta-00002")
  (check-true (pi-pool-key-cooling? alpha (pi-current-time-millis)))
  (check (pi-multikey-request-key cfg) => "sk-pool-beta-00002")
) ;let*

;; 2. 403 使用 invalidKeyCooldownMs（fixture 中为 300000ms）
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (cfg (list (cons 'multikey-pool pool) (cons 'api-key "sk-pool-alpha-0001")))
       (next (pi-multikey-rotate! cfg "sk-pool-alpha-0001" 403))
       (alpha (car (pi-key-pool-keys pool)))
       (now (pi-current-time-millis))
      ) ;
  (check next => "sk-pool-beta-00002")
  (check-true (pi-pool-key-cooling? alpha (+ now 299000)))
  (check-true (not (pi-pool-key-cooling? alpha (+ now 301000))))
) ;let*

;; 3. "invalid" 等价于 403
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (cfg (list (cons 'multikey-pool pool) (cons 'api-key "sk-pool-alpha-0001")))
       (next (pi-multikey-rotate! cfg "sk-pool-alpha-0001" "invalid"))
      ) ;
  (check next => "sk-pool-beta-00002")
) ;let*

;; 4. 三个 key 按故障顺序切换：key-1 → key-2 → key-3 → exhausted
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (cfg (list (cons 'multikey-pool pool) (cons 'api-key "sk-pool-alpha-0001")))
       (second (pi-multikey-rotate! cfg "sk-pool-alpha-0001" 429))
       (third (pi-multikey-rotate! cfg second 403))
       (exhausted (pi-multikey-rotate! cfg third 429))
      ) ;
  (check second => "sk-pool-beta-00002")
  (check third => "sk-pool-delta-00004")
  (check exhausted => 'exhausted)
) ;let*

;; 5. 两个 key 逐个耗尽：最后一个 key 被冷却后返回 'exhausted
(let* ((pool (pi-load-multikey fixture-dir "google"))
       (cfg (list (cons 'multikey-pool pool) (cons 'api-key "goog-key-0000000001")))
       (first-next (pi-multikey-rotate! cfg "goog-key-0000000001" 429))
       (second-next (pi-multikey-rotate! cfg first-next 403))
      ) ;
  (check first-next => "goog-key-0000000002")
  (check second-next => 'exhausted)
) ;let*

;; 6. 无 pool：返回 #f，不改变任何状态
(check (pi-multikey-rotate! (list (cons 'multikey-pool #f)
                                (cons 'api-key "auth-json-key")
                              ) ;list
         "auth-json-key"
         429
       ) ;pi-multikey-rotate!
  => #f
) ;check

(check-report)
