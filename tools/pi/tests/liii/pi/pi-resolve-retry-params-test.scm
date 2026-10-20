(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-resolve-retry-params
;; 解析 settings 的 retry 节，返回四个值：
;;   max-retries base-delay-ms max-agent-delay-ms provider-max-retries
;; retry.provider.maxRetries（settings.json 嵌套节，键 provider-max-retries）
;; 作为 provider 级错误（429/5xx/408/409/网络错误）的重试预算，
;; 未配置时回退顶层 max-retries。

;; 1. 空 settings：全部缺省，provider 预算回退顶层 3
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params '() #f)))
  (check max-retries => 3)
  (check base-delay-ms => 2000)
  (check max-agent-delay-ms => 60000)
  (check provider-max-retries => 3))

;; 2. 嵌套 retry.provider.maxRetries 命中（顶层预算保持 3）
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params
                '((retry (enabled . #t)
                         (max-retries . 3)
                         (base-delay-ms . 2000)
                         (max-agent-delay-ms . 60000)
                         (provider (max-retries . 12))))
                #f)))
  (check max-retries => 3)
  (check base-delay-ms => 2000)
  (check max-agent-delay-ms => 60000)
  (check provider-max-retries => 12))

;; 3. 无嵌套节：回退顶层 max-retries（含自定义顶层值）
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params '((retry (max-retries . 5))) #f)))
  (check max-retries => 5)
  (check provider-max-retries => 5))

;; 4. enabled=#f：顶层与 provider 预算均为 0
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params
                '((retry (enabled . #f) (max-retries . 3) (provider (max-retries . 12))))
                #f)))
  (check max-retries => 0)
  (check provider-max-retries => 0))

;; 5. opt-retry 覆盖：provider 预算经 opt-retry 提供
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params '()
                '((enabled . #t) (max-retries . 3) (provider-max-retries . 8)))))
  (check max-retries => 3)
  (check provider-max-retries => 8))

;; 6. opt-retry 未给 provider 预算：回退 opt-retry 的顶层值
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params '()
                '((enabled . #t) (max-retries . 4) (base-delay-ms . 100)))))
  (check max-retries => 4)
  (check provider-max-retries => 4))

;; 7. settings 为 pi-settings record（嵌套节经 record 保留）
(let* ((retry (pi-retry-config #t 3 2000 60000 12))
       (settings (pi-settings "google" "gemini-3.8-flash" "off" #f retry #f)))
  (let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
                (pi-resolve-retry-params settings #f)))
    (check max-retries => 3)
    (check provider-max-retries => 12)))

(check-report)
