(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-multikey-request-key
;; 每次请求开始时选取本次应使用的 key：
;;   有 pool 时按当前 cooldown/LRU 状态重新选取（保证跨轮次不复用已冷却 key）；
;;   无 pool 时回退 config 中的 api-key（既有 auth.json 路径）。

(define fixture-dir
  (if (file-exists? "tests/fixtures/multikey")
    "tests/fixtures/multikey"
    "tools/pi/tests/fixtures/multikey"
  ) ;if
) ;define

;; 1. 无 pool：原样返回 config 的 api-key
(check (pi-multikey-request-key (list (cons 'multikey-pool #f)
                                   (cons 'api-key "auth-json-key")
                                 ) ;list
       ) ;pi-multikey-request-key
  => "auth-json-key"
) ;check

;; 2. 有 pool：按 LRU 选取，连续两次请求轮换到不同 key
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (cfg (list (cons 'multikey-pool pool) (cons 'api-key "auth-json-key")))
      ) ;
  (check (pi-multikey-request-key cfg) => "sk-pool-alpha-0001")
  (check (pi-multikey-request-key cfg) => "sk-pool-beta-00002")
  (check (pi-multikey-request-key cfg) => "sk-pool-alpha-0001")
) ;let*

;; 3. 冷却中的 key 不被 request-key 选中
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (cfg (list (cons 'multikey-pool pool) (cons 'api-key "auth-json-key")))
       (alpha (car (pi-key-pool-keys pool)))
      ) ;
  (pi-multikey-mark-cooldown! alpha 429 (pi-current-time-millis) pool)
  (check (pi-multikey-request-key cfg) => "sk-pool-beta-00002")
) ;let*

(check-report)
