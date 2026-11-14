;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code all-in-one types)
  (import (scheme base) (liii list) (liii string) (liii pi code config user-fs))
  (export make-chain-node
    chain-node?
    chain-node-provider
    chain-node-model
    chain-node-thinking-level
    json->chain-node

    make-chain-def
    chain-def?
    chain-def-id
    chain-def-name
    chain-def-nodes
    json->chain-def

    make-all-in-one-cfg
    all-in-one-cfg?
    all-in-one-cfg-continuous?
    all-in-one-cfg-max-wait-ms
    all-in-one-cfg-retry-interval-ms
    all-in-one-cfg-probe-timeout-ms
    all-in-one-cfg-chains
    json->all-in-one-cfg
  ) ;export
  (begin
    ;; 极简三元组节点
    (define-record-type <chain-node>
      (%make-chain-node provider model thinking-level)
      chain-node?
      (provider chain-node-provider)
      (model chain-node-model)
      (thinking-level chain-node-thinking-level)
    ) ;define-record-type

    (define* (make-chain-node provider model (thinking-level "off"))
      (%make-chain-node provider model thinking-level)
    ) ;define*

    (define (%non-empty-string? x)
      (and (string? x) (not (string-null? (string-trim-both x))))
    ) ;define

    (define (%json-obj obj)
      (if (equal? obj '(())) '() obj)
    ) ;define

    (define (json->chain-node obj . opt-chain-id)
      (if (chain-node? obj)
        obj
        (let ((chain-id (if (pair? opt-chain-id) (car opt-chain-id) "<unknown>")))
          (if (not (pair? obj))
            (begin
              (pi-user-warn (string-append "all-in-one: chain \"" chain-id "\" 的 node 必须是对象")
              ) ;pi-user-warn
              #f
            ) ;begin
            (let* ((data (%json-obj obj))
                   (p (pi-user-dict-ref data "provider" #f))
                   (m (pi-user-dict-ref data "model" #f))
                   (tl-raw (pi-user-dict-ref data "thinkingLevel" "off"))
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

    ;; 链式定义 (链式 ID 即伪模型 ID)
    (define-record-type <chain-def>
      (%make-chain-def id name nodes)
      chain-def?
      (id chain-def-id)
      (name chain-def-name)
      (nodes chain-def-nodes)
    ) ;define-record-type

    (define (make-chain-def id name nodes)
      (%make-chain-def id name nodes)
    ) ;define

    (define (json->chain-def obj)
      (if (chain-def? obj)
        obj
        (if (not (pair? obj))
          (begin
            (pi-user-warn "all-in-one: chain 必须是对象")
            #f
          ) ;begin
          (let* ((data (%json-obj obj))
                 (id (pi-user-dict-ref data "id" #f))
                 (name (pi-user-dict-ref data "name" #f))
                 (raw-nodes (pi-user-dict-ref data "nodes" '()))
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
                           (pi-user-warn (string-append "all-in-one: chain \"" id "\" 的 nodes 必须是数组")
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

    ;; 全局配置
    (define-record-type <all-in-one-cfg>
      (%make-all-in-one-cfg continuous? max-wait-ms retry-interval-ms
        probe-timeout-ms chains
      ) ;%make-all-in-one-cfg
      all-in-one-cfg?
      (continuous? all-in-one-cfg-continuous?)
      (max-wait-ms all-in-one-cfg-max-wait-ms)
      (retry-interval-ms all-in-one-cfg-retry-interval-ms)
      (probe-timeout-ms all-in-one-cfg-probe-timeout-ms)
      (chains all-in-one-cfg-chains)
    ) ;define-record-type

    (define (make-all-in-one-cfg continuous? max-wait-ms retry-interval-ms
              probe-timeout-ms chains
            ) ;make-all-in-one-cfg
      (%make-all-in-one-cfg continuous? max-wait-ms retry-interval-ms
        probe-timeout-ms chains
      ) ;%make-all-in-one-cfg
    ) ;define

    ;; 解析 JSON 布尔值（支持 #t/#f 与符号 'true/'false）
    (define (%json-boolean raw default-val)
      (cond ((boolean? raw) raw)
            ((eq? raw 'true) #t)
            ((eq? raw 'false) #f)
            (else default-val)
      ) ;cond
    ) ;define

    ;; 解析秒或毫秒数（秒数乘以 1000 转换为内部毫秒数；支持多种秒级键名）
    (define (%parse-duration-ms data sec-keys ms-key default-ms)
      (let loop
        ((keys (if (list? sec-keys) sec-keys (list sec-keys))))
        (if (pair? keys)
          (let ((val (pi-user-dict-ref data (car keys) #f)))
            (if (and (number? val) (> val 0))
              (inexact->exact (round (* val 1000)))
              (loop (cdr keys))
            ) ;if
          ) ;let
          (let ((ms (pi-user-dict-ref data ms-key #f)))
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
        (if (not (pair? obj))
          #f
          (let* ((data (%json-obj obj))
                 (cfg-path (if (pair? opt-cfg-path) (car opt-cfg-path) "all-in-one.json"))
                 (cont-raw (pi-user-dict-ref data "continuous" #t))
                 (cont (%json-boolean cont-raw #t))
                 (max-wait (%parse-duration-ms data '("maxWait"
                                                      "maxWaitSeconds") "maxWaitMs" 3600000)
                 ) ;max-wait
                 (retry-interval (%parse-duration-ms data
                                   '("retryInterval" "cooldown"
                                     "retryIntervalSeconds" "cooldownSeconds")
                                   "retryIntervalMs"
                                   60000
                                 ) ;%parse-duration-ms
                 ) ;retry-interval
                 (probe-timeout (%parse-duration-ms data
                                  '("probeTimeout" "probeTimeoutSeconds")
                                  "probeTimeoutMs"
                                  8000
                                ) ;%parse-duration-ms
                 ) ;probe-timeout
                 (raw-chains (pi-user-dict-ref data "chains" '()))
                 (chains (%parse-chains cfg-path raw-chains))
                ) ;
            (and chains
              (make-all-in-one-cfg cont max-wait retry-interval probe-timeout chains)
            ) ;and
          ) ;let*
        ) ;if
      ) ;if
    ) ;define
  ) ;begin
) ;define-library
