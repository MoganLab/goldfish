;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code all-in-one types)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii json)
    (liii list)
    (liii string)
    (liii pi code config user-fs)
  ) ;import
  (export make-chain-node
    chain-node?
    chain-node-provider
    chain-node-model
    chain-node-thinking-level
    json->chain-node
    chain-node->json

    make-chain-def
    chain-def?
    chain-def-id
    chain-def-name
    chain-def-nodes
    json->chain-def
    chain-def->json

    make-all-in-one-cfg
    all-in-one-cfg?
    all-in-one-cfg-continuous?
    all-in-one-cfg-max-wait-ms
    all-in-one-cfg-retry-interval-ms
    all-in-one-cfg-probe-timeout-ms
    all-in-one-cfg-chains
    json->all-in-one-cfg
    all-in-one-cfg->json
  ) ;export
  (begin
    ;; 极简三元组节点
    (define*-record-type <chain-node>
      (make-chain-node provider model (thinking-level "off"))
      chain-node?
      (provider chain-node-provider)
      (model chain-node-model)
      (thinking-level chain-node-thinking-level)
    ) ;define*-record-type

    (define (%non-empty-string? x)
      (and (string? x) (not (string-null? (string-trim-both x))))
    ) ;define

    (define (json->chain-node obj . opt-chain-id)
      (if (chain-node? obj)
        obj
        (let ((chain-id (if (pair? opt-chain-id) (car opt-chain-id) "<unknown>")))
          (if (not (json-object? obj))
            (begin
              (pi-user-warn (string-append "all-in-one: chain \"" chain-id "\" 的 node 必须是对象")
              ) ;pi-user-warn
              #f
            ) ;begin
            (let* ((p (json-ref-string obj "provider" #f))
                   (m (json-ref-string obj "model" #f))
                   (tl-raw (json-ref-string obj "thinkingLevel" "off"))
                   (tl (if (%non-empty-string? tl-raw) tl-raw "off"))
                  ) ;
              (cond ((not (%non-empty-string? p))
                     (pi-user-warn (string-append "all-in-one: chain \""
                                     chain-id
                                     "\" 节点缺少必填字段 provider，已跳过"
                                   ) ;string-append
                     ) ;pi-user-warn
                     #f
                    ) ;
                    ((not (%non-empty-string? m))
                     (pi-user-warn (string-append "all-in-one: chain \""
                                     chain-id
                                     "\" 节点缺少必填字段 model，已跳过"
                                   ) ;string-append
                     ) ;pi-user-warn
                     #f
                    ) ;
                    (else (make-chain-node p m tl))
              ) ;cond
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (chain-node->json node)
      (unless (chain-node? node)
        (type-error "chain-node->json: expected chain-node" node)
      ) ;unless
      (list (cons "provider" (chain-node-provider node))
        (cons "model" (chain-node-model node))
        (cons "thinkingLevel" (chain-node-thinking-level node))
      ) ;list
    ) ;define

    ;; 链式定义 (链式 ID 即伪模型 ID)
    (define*-record-type <chain-def>
      (make-chain-def id name (nodes '()))
      chain-def?
      (id chain-def-id)
      (name chain-def-name)
      (nodes chain-def-nodes)
    ) ;define*-record-type

    (define (json->chain-def obj)
      (if (chain-def? obj)
        obj
        (if (not (json-object? obj))
          (begin
            (pi-user-warn "all-in-one: chain 必须是对象")
            #f
          ) ;begin
          (let* ((id (json-ref-string obj "id" #f))
                 (name (json-ref-string obj "name" #f))
                 (raw-nodes (json-ref obj "nodes"))
                 (nodes-list (pi-user-vector->list raw-nodes))
                ) ;
            (if (not (%non-empty-string? id))
              (begin
                (pi-user-warn "all-in-one: chain 缺少必填字段 id")
                #f
              ) ;begin
              (let* ((eff-name (if (%non-empty-string? name) name id))
                     (valid-nodes
                       (if (not (list? nodes-list))
                         (begin
                           (pi-user-warn (string-append "all-in-one: chain \"" (or id "") "\" 的 nodes 必须是数组")
                           ) ;pi-user-warn
                           '()
                         ) ;begin
                         (filter (lambda (x) x) (map (lambda (n) (json->chain-node n id)) nodes-list))
                       ) ;if
                     ) ;valid-nodes
                    ) ;
                (make-chain-def id eff-name valid-nodes)
              ) ;let*
            ) ;if
          ) ;let*
        ) ;if
      ) ;if
    ) ;define

    (define (chain-def->json chain)
      (unless (chain-def? chain)
        (type-error "chain-def->json: expected chain-def" chain)
      ) ;unless
      (list (cons "id" (chain-def-id chain))
        (cons "name" (chain-def-name chain))
        (cons "nodes" (list->vector (map chain-node->json (chain-def-nodes chain))))
      ) ;list
    ) ;define

    ;; 全局配置
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

    ;; 解析秒或毫秒数（秒数乘以 1000 转换为内部毫秒数；支持多种秒级键名）
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

    ;; 解析 chains 数组，返回有效 <chain-def> 列表；格式非法时返回 #f
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
