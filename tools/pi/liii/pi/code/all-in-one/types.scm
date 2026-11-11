;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code all-in-one types)
  (import (scheme base))
  (export make-chain-node
    chain-node?
    chain-node-provider
    chain-node-model
    chain-node-thinking-level

    make-chain-def
    chain-def?
    chain-def-id
    chain-def-name
    chain-def-nodes

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
