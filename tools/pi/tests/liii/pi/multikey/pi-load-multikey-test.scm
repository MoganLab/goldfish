(import (liii check) (liii pi multikey pool) (liii pi multikey load))

(check-set-mode! 'report-failed)

;; pi-load-multikey
;; 读取 <agent-dir>/multikey.json 的密钥池配置。
;; 存在与 provider 对应的 pool 时返回该 pool 的密钥池 record，
;; 过滤 enabled: false 的 key；无对应 pool 或文件不存在时返回 #f。

(define fixture-dir
  (if (file-exists? "tests/fixtures/multikey")
    "tests/fixtures/multikey"
    "tools/pi/tests/fixtures/multikey"
  ) ;if
) ;define

(define direct-dir
  (if (file-exists? "tests/fixtures/direct")
    "tests/fixtures/direct"
    "tools/pi/tests/fixtures/direct"
  ) ;if
) ;define

;; 1. 命中 pool：返回 record，enabled=false 的 gamma 被过滤
(let ((pool (pi-load-multikey fixture-dir "kimi-coding")))
  (check-true (pi-key-pool? pool))
  (check (pi-key-pool-id pool) => "kimi-coding")
  (check (length (pi-key-pool-keys pool)) => 3)
  (check (pi-key-pool-cooldown-ms pool) => 30000)
  (check (pi-key-pool-invalid-cooldown-ms pool) => 300000)
  (let ((first-key (car (pi-key-pool-keys pool))))
    (check (pi-pool-key-key first-key) => "sk-pool-alpha-0001")
    (check (pi-pool-key-label first-key) => "alpha")
    (check-true (pi-pool-key-enabled first-key))
    ;; 初始状态：不在冷却期，last-used 为 0，配置顺序决定首个 key
    (check-true (not (pi-pool-key-cooling? first-key 1000000)))
    (check (pi-pool-key-last-used first-key) => 0)
  ) ;let
) ;let

;; 2. pool 未配置冷却时长：429 默认 20000ms，403/invalid 默认 600000ms
(let ((pool (pi-load-multikey fixture-dir "google")))
  (check-true (pi-key-pool? pool))
  (check (pi-key-pool-cooldown-ms pool) => 20000)
  (check (pi-key-pool-invalid-cooldown-ms pool) => 600000)
  (check (length (pi-key-pool-keys pool)) => 2)
  (check (pi-pool-key-label (car (pi-key-pool-keys pool))) => "g1")
) ;let

;; 3. 无对应 provider 的 pool：返回 #f
(check (pi-load-multikey fixture-dir "anthropic") => #f)

;; 4. multikey.json 不存在：返回 #f（回退 auth.json 由调用方处理）
(check (pi-load-multikey direct-dir "google") => #f)

;; 5. 全部 key 被禁用的 pool 视为无 pool
(let ((bad-dir
        (if (file-exists? "tests/fixtures/multikey-disabled")
          "tests/fixtures/multikey-disabled"
          "tools/pi/tests/fixtures/multikey-disabled"
        ) ;if
      ) ;bad-dir
     ) ;
  (check (pi-load-multikey bad-dir "google") => #f)
) ;let

(check-report)
