;; pi-resolve-retry-params
;; 解析配置中的重试参数，返回四个重试预算值。
;;
;; 语法
;; ----
;; (pi-resolve-retry-params settings opt-retry)
;;
;; 参数
;; ----
;; settings : list?
;; 配置项列表。
;;
;; opt-retry : any?
;; 显式指定的重试配置或覆盖选项。
;;
;; 返回值
;; ----
;; values
;; 多值返回：max-retries base-delay-ms max-agent-delay-ms provider-max-retries。

(import (liii check) (liii pi ai config record) (liii pi ai retry))

(check-set-mode! 'report-failed)

;; 1. 空 settings：全部缺省，provider 预算回退顶层 3
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params '() #f)
             ) ;
            ) ;
  (check max-retries => 3)
  (check base-delay-ms => 2000)
  (check max-agent-delay-ms => 60000)
  (check provider-max-retries => 3)
) ;let-values

;; 2. 嵌套 retry.provider.maxRetries 命中（顶层预算保持 3）
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params
                '((retry (enabled . #t)
                    (max-retries . 3)
                    (base-delay-ms . 2000)
                    (max-agent-delay-ms . 60000)
                    (provider (max-retries . 12))))
                #f
              ) ;pi-resolve-retry-params
             ) ;
            ) ;
  (check max-retries => 3)
  (check base-delay-ms => 2000)
  (check max-agent-delay-ms => 60000)
  (check provider-max-retries => 12)
) ;let-values

;; 3. 无嵌套节：回退顶层 max-retries（含自定义顶层值）
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params '((retry (max-retries . 5))) #f)
             ) ;
            ) ;
  (check max-retries => 5)
  (check provider-max-retries => 5)
) ;let-values

;; 4. enabled=#f：顶层与 provider 预算均为 0
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params
                '((retry (enabled . #f)
                    (max-retries . 3)
                    (provider (max-retries . 12))))
                #f
              ) ;pi-resolve-retry-params
             ) ;
            ) ;
  (check max-retries => 0)
  (check provider-max-retries => 0)
) ;let-values

;; 5. opt-retry 覆盖：provider 预算经 opt-retry 提供
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params '() '((enabled . #t)
                                             (provider (max-retries . 8))))
             ) ;
            ) ;
  (check max-retries => 3)
  (check provider-max-retries => 8)
) ;let-values

;; 6. opt-retry 无 provider 节：回退 opt-retry 顶层
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params '() '((enabled . #t) (max-retries . 6)))
             ) ;
            ) ;
  (check max-retries => 6)
  (check provider-max-retries => 6)
) ;let-values

;; 7. record 形态 settings：嵌套 provider-max-retries 正常传递
(let* ((cfg (pi-retry-config
              :enabled              #t
              :max-retries          4
              :base-delay-ms        1000
              :max-agent-delay-ms   30000
              :provider-max-retries 16
            ) ;pi-retry-config
       ) ;cfg
       (settings (pi-settings :retry cfg))
      ) ;
  (let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
                (pi-resolve-retry-params settings #f)
               ) ;
              ) ;
    (check max-retries => 4)
    (check base-delay-ms => 1000)
    (check max-agent-delay-ms => 30000)
    (check provider-max-retries => 16)
  ) ;let-values
) ;let*

(check-report)
