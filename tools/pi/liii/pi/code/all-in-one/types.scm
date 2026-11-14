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
            (let* ((p (pi-user-dict-ref obj "provider" #f))
                   (m (pi-user-dict-ref obj "model" #f))
                   (tl-raw (pi-user-dict-ref obj "thinkingLevel" "off"))
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
          (let* ((id (pi-user-dict-ref obj "id" #f))
                 (name (pi-user-dict-ref obj "name" #f))
                 (raw-nodes (pi-user-dict-ref obj "nodes" '()))
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
  ) ;begin
) ;define-library
