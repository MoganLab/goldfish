;; pi-multikey-request-key
;; 为本次请求选取应使用的密钥。
;;
;; 语法
;; ----
;; (pi-multikey-request-key config)
;;
;; 参数
;; ----
;; config : list?
;; 会话配置 alist，可含 multikey-pool 与 api-key 字段。
;;
;; 返回值
;; ----
;; (or string? #f)
;; 本次请求使用的密钥字符串，无可用密钥时返回 #f。

(import (liii check) (liii pi ai time) (liii pi multikey pool) (liii pi multikey load) (liii pi multikey policy))

(check-set-mode! 'report-failed)

;; 每次请求开始时选取本次应使用的 key：
;;   有 pool 时优先复用最近使用的可用 key，正常连续请求不切换；
;;   当前 key 冷却后才切换，无 pool 时回退 config 中的 api-key。

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

;; 2. 有 pool：正常连续请求固定使用 key-1
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (cfg (list (cons 'multikey-pool pool) (cons 'api-key "auth-json-key")))
      ) ;
  (check (pi-multikey-request-key cfg) => "sk-pool-alpha-0001")
  (check (pi-multikey-request-key cfg) => "sk-pool-alpha-0001")
  (check (pi-multikey-request-key cfg) => "sk-pool-alpha-0001")
) ;let*

;; 3. 当前 key 冷却后才切换，后续请求持续使用 key-2
(let* ((pool (pi-load-multikey fixture-dir "kimi-coding"))
       (cfg (list (cons 'multikey-pool pool) (cons 'api-key "auth-json-key")))
       (alpha (car (pi-key-pool-keys pool)))
      ) ;
  (pi-multikey-mark-cooldown! alpha 429 (pi-current-time-millis) pool)
  (check (pi-multikey-request-key cfg) => "sk-pool-beta-00002")
  (check (pi-multikey-request-key cfg) => "sk-pool-beta-00002")
) ;let*

(check-report)
