;; pi-probe-retry-config
;; 计算 all-in-one 节点探活的���试预算配置。
;;
;; 语法
;; ----
;; (pi-probe-retry-config settings)
;;
;; 参数
;; ----
;; settings : <pi-settings> | #f
;; 用户会话设置；为 #f 或缺少 retry 段时使用缺省行为。
;;
;; 返回值
;; ----
;; alist
;; 形如 ((enabled . #t) (max-retries . 3) (base-delay-ms . 2000)
;;       (max-agent-delay-ms . 10000)) 的重试覆盖配置；
;; 用户显式禁用重试时返回 ((enabled . #f))。

(import (scheme base)
  (liii check)
  (liii pi ai config record)
  (liii pi code all-in-one probe)
) ;import

(check-set-mode! 'report-failed)

;; 1. 无 settings：缺省启用、最多 3 次重试、退避基数 2000ms、单次退避上限 10s
(let ((cfg (pi-probe-retry-config #f)))
  (check (cdr (assoc 'enabled cfg)) => #t)
  (check (cdr (assoc 'max-retries cfg)) => 3)
  (check (cdr (assoc 'base-delay-ms cfg)) => 2000)
  (check (cdr (assoc 'max-agent-delay-ms cfg)) => 10000)
) ;let

;; 2. 用户配置了 retry：退避基数沿用用户配置，但重试次数固定为 3 次
(let ((cfg (pi-probe-retry-config
              (pi-settings "google" "gemini-3.8-flash" #f #f
                (pi-retry-config #t 2 500 60000 12)
              ) ;pi-settings
            )))
  (check (cdr (assoc 'enabled cfg)) => #t)
  (check (cdr (assoc 'max-retries cfg)) => 3)
  (check (cdr (assoc 'base-delay-ms cfg)) => 500)
  (check (cdr (assoc 'max-agent-delay-ms cfg)) => 10000)
) ;let

;; 3. 用户退避基数过大时按 2000ms 封顶，避免探活等待过长
(let ((cfg (pi-probe-retry-config
              (pi-settings "google" "gemini-3.8-flash" #f #f
                (pi-retry-config #t 3 30000 60000 12)
              ) ;pi-settings
            )))
  (check (cdr (assoc 'base-delay-ms cfg)) => 2000)
) ;let

;; 4. 用户显式禁用重试：探活不重试
(let ((cfg (pi-probe-retry-config
              (pi-settings "google" "gemini-3.8-flash" #f #f
                (pi-retry-config #f 3 2000 60000 #f)
              ) ;pi-settings
            )))
  (check cfg => '((enabled . #f)))
) ;let

(check-report)
