;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `<all-in-one-cfg>` 领域记录：all-in-one 全局调度参数与链定义列表，
;; 记录定义、构造器、访问器、秒/毫秒时长解析与 JSON 双向转换放在同一文件。
(define-library (liii pi code all-in-one config)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii json)
    (liii list)
    (liii string)
    (liii pi code all-in-one chain-def)
    (liii pi code config user-fs)
  ) ;import
  ;; 构造
  (export make-all-in-one-cfg)
  ;; 谓词
  (export all-in-one-cfg?)
  ;; 属性访问
  (export all-in-one-cfg-continuous? all-in-one-cfg-max-wait-ms
    all-in-one-cfg-retry-interval-ms all-in-one-cfg-probe-timeout-ms
    all-in-one-cfg-chains
  ) ;export
  ;; JSON 转换
  (export all-in-one-cfg->json json->all-in-one-cfg)
  (begin

    (define*-record-type <all-in-one-cfg>
      (make-all-in-one-cfg (continuous? #t)
        (max-wait-ms 3600000)
        (retry-interval-ms 60000)
        (probe-timeout-ms 8000)
        (chains '())
      ) ;make-all-in-one-cfg
      all-in-one-cfg?
      (continuous? all-in-one-cfg-continuous?)
      (max-wait-ms all-in-one-cfg-max-wait-ms)
      (retry-interval-ms all-in-one-cfg-retry-interval-ms)
      (probe-timeout-ms all-in-one-cfg-probe-timeout-ms)
      (chains all-in-one-cfg-chains)
    ) ;define*-record-type

    ;; 解析秒或毫秒数：秒级键名（可多个别名）优先并乘 1000，其次毫秒键，最后默认值。
    (define (%parse-duration-ms data sec-keys ms-key default-ms)
      (let loop
        ((keys (if (list? sec-keys) sec-keys (list sec-keys))))
        (if (pair? keys)
          (let ((val (json-ref data (car keys))))
            (if (and (number? val) (> val 0))
              (inexact->exact (round (* val 1000)))
              (loop (cdr keys))
            ) ;if
          ) ;let
          (let ((ms (json-ref data ms-key)))
            (if (and (number? ms) (> ms 0)) ms default-ms)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 解析 chains 数组；根不是数组时告警并返回 #f，由调用方折叠成回退。
    (define (%parse-chains cfg-path raw-chains)
      (let ((chains-list (pi-user-vector->list raw-chains)))
        (if (not (list? chains-list))
          (begin
            (pi-user-warn (string-append cfg-path ": chains 必须是数组"))
            #f
          ) ;begin
          (filter (lambda (x) x) (map json->chain-def chains-list))
        ) ;if
      ) ;let
    ) ;define

    (define (json->all-in-one-cfg obj . opt-cfg-path)
      (if (all-in-one-cfg? obj)
        obj
        (if (not (json-object? obj))
          #f
          (let* ((cfg-path (if (pair? opt-cfg-path) (car opt-cfg-path) "all-in-one.json"))
                 (cont (json-ref-boolean obj "continuous" #t))
                 (max-wait (%parse-duration-ms obj '("maxWait" "maxWaitSeconds") "maxWaitMs" 3600000)
                 ) ;max-wait
                 (retry-interval (%parse-duration-ms obj
                                   '("retryInterval" "cooldown"
                                     "retryIntervalSeconds" "cooldownSeconds")
                                   "retryIntervalMs"
                                   60000
                                 ) ;%parse-duration-ms
                 ) ;retry-interval
                 (probe-timeout (%parse-duration-ms obj
                                  '("probeTimeout" "probeTimeoutSeconds")
                                  "probeTimeoutMs"
                                  8000
                                ) ;%parse-duration-ms
                 ) ;probe-timeout
                 (raw-chains (json-ref obj "chains"))
                 (chains (%parse-chains cfg-path raw-chains))
                ) ;
            (and chains
              (make-all-in-one-cfg cont max-wait retry-interval probe-timeout chains)
            ) ;and
          ) ;let*
        ) ;if
      ) ;if
    ) ;define

    (define (all-in-one-cfg->json cfg)
      (unless (all-in-one-cfg? cfg)
        (type-error "all-in-one-cfg->json: expected all-in-one-cfg" cfg)
      ) ;unless
      (list (cons "provider" "all-in-one")
        (cons "continuous" (if (all-in-one-cfg-continuous? cfg) 'true 'false))
        (cons "maxWaitMs" (all-in-one-cfg-max-wait-ms cfg))
        (cons "retryIntervalMs" (all-in-one-cfg-retry-interval-ms cfg))
        (cons "probeTimeoutMs" (all-in-one-cfg-probe-timeout-ms cfg))
        (cons "chains" (list->vector (map chain-def->json (all-in-one-cfg-chains cfg))))
      ) ;list
    ) ;define

  ) ;begin
) ;define-library
