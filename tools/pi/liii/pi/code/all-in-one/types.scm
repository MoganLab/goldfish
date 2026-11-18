;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;
;; All-in-one 元供应商的领域记录聚合入口。

;; 只重导出平铺 record 模块的正式领域接口：
;;   chain-node.scm / chain-def.scm / config.scm 各自承载 record 定义、构造器、
;;   访问器与 JSON 双向转换，types 不再保存任何重复实现。
(define-library (liii pi code all-in-one types)
  (import (scheme base)
    (liii pi code all-in-one chain-node)
    (liii pi code all-in-one chain-def)
    (liii pi code all-in-one config)
  ) ;import

  ;; ---------- 链节点记录 <chain-node> ----------

  ;; 构造
  (export make-chain-node)
  ;; 谓词
  (export chain-node?)
  ;; 属性访问
  (export chain-node-provider chain-node-model chain-node-thinking-level)
  ;; JSON 转换
  (export chain-node->json json->chain-node)

  ;; ---------- 链定义记录 <chain-def> ----------

  ;; 构造
  (export make-chain-def)
  ;; 谓词
  (export chain-def?)
  ;; 属性访问
  (export chain-def-id chain-def-name chain-def-nodes)
  ;; JSON 转换
  (export chain-def->json json->chain-def)

  ;; ---------- 全局配置记录 <all-in-one-cfg> ----------

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
) ;define-library
