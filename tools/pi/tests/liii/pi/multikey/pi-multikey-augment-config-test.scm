(import (liii check) (liii pi ai keyring)
  (liii pi multikey pool) (liii pi multikey config))

(check-set-mode! 'report-failed)

;; pi-multikey-augment-config
;; config augmenter：命中 <config-dir>/multikey.json 中 provider 对应的 pool 时
;; 注入 api-key（首个池内 key）/ multikey-pool / request-key / rotate-key；
;; 未命中时原样返回 cfg，核心经 (liii pi ai keyring) 钩子消费。

(define fixture-dir
  (if (file-exists? "tests/fixtures/multikey")
    "tests/fixtures/multikey"
    "tools/pi/tests/fixtures/multikey"
  ) ;if
) ;define

(define nopool-dir
  (if (file-exists? "tests/fixtures/direct")
    "tests/fixtures/direct"
    "tools/pi/tests/fixtures/direct"
  ) ;if
) ;define

;; 1. 命中 pool：api-key 换成首个池内 key，注入钩子，其余条目保留
(let* ((base (list (cons 'agent-dir fixture-dir)
                   (cons 'provider "kimi-coding")
                   (cons 'api-key "auth-json-key")
                   (cons 'model-id "kimi-k2")
              ) ;list
       ) ;base
       (cfg (pi-multikey-augment-config base '()))
      ) ;
  (check (cdr (assoc 'api-key cfg)) => "sk-pool-alpha-0001")
  (check-true (pi-key-pool? (cdr (assoc 'multikey-pool cfg))))
  (check-true (procedure? (cdr (assoc 'request-key cfg))))
  (check-true (procedure? (cdr (assoc 'rotate-key cfg))))
  ;; 原有条目保留
  (check (cdr (assoc 'model-id cfg)) => "kimi-k2")
  ;; 核心钩子接口在增强后的 cfg 上生效
  (check-true (pi-key-rotation? cfg))
  (check (pi-request-api-key cfg) => "sk-pool-alpha-0001")
  ;; 429 轮换到下一可用 key
  (check (pi-rotate-api-key! cfg "sk-pool-alpha-0001" 429) => "sk-pool-beta-00002")
) ;let*

;; 1.1 config-dir 优先：JSON 配置文件目录（~/.gf/agent 等）中的 multikey.json 生效
(let* ((base (list (cons 'config-dir fixture-dir)
                   (cons 'agent-dir nopool-dir)
                   (cons 'provider "kimi-coding")
                   (cons 'api-key "auth-json-key")
                   (cons 'model-id "kimi-k2")
              ) ;list
       ) ;base
       (cfg (pi-multikey-augment-config base '()))
      ) ;
  (check (cdr (assoc 'api-key cfg)) => "sk-pool-alpha-0001")
  (check-true (pi-key-pool? (cdr (assoc 'multikey-pool cfg))))
) ;let*

;; 2. 无 multikey.json 或 provider 无 pool：cfg 原样返回，不注入钩子
(let* ((base (list (cons 'agent-dir nopool-dir)
                   (cons 'provider "google")
                   (cons 'api-key "auth-json-key")
              ) ;list
       ) ;base
       (cfg (pi-multikey-augment-config base '()))
      ) ;
  (check-true (eq? cfg base))
  (check (cdr (assoc 'api-key cfg)) => "auth-json-key")
  (check-true (not (assoc 'multikey-pool cfg)))
  (check-true (not (pi-key-rotation? cfg)))
  (check (pi-request-api-key cfg) => "auth-json-key")
) ;let*

;; 3. provider 缺失或 cfg 非法：安全回退原样返回
(check (pi-multikey-augment-config (list (cons 'api-key "k")) '())
  => (list (cons 'api-key "k"))
) ;check
(check-true (eq? (pi-multikey-augment-config 'foo '()) 'foo))

(check-report)
