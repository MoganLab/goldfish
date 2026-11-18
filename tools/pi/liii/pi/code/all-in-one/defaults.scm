;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; all-in-one 默认配置的唯一真值源：
;;   默认链（默认伪模型）与默认全局调度参数只在这里构造，
;;   provider 目录回退、request/compaction 回退、配置 TUI 回退与
;;   首次生成的 all-in-one.json 全部由本模块派生，不再各自手写一份。
(define-library (liii pi code all-in-one defaults)
  (import (scheme base) (liii pi code all-in-one types))
  (export all-in-one-default-config-name
    all-in-one-default-chains
    make-default-all-in-one-config
  ) ;export
  (begin

    ;; 默认配置文件的展示名称，仅在首次生成 all-in-one.json 时写入。
    (define all-in-one-default-config-name "高可用长程推进总线")

    ;; 默认双链：快速推进链 google -> minimax-cn -> deepseek，
    ;; 高质攻坚链 kimi-coding -> zai-coding-cn。
    (define (all-in-one-default-chains)
      (list (make-chain-def "coding-fast"
              "快速"
              (list (make-chain-node "google" "gemini-3.8-flash" "medium")
                (make-chain-node "minimax-cn" "MiniMax-M3.1-Flash" "medium")
                (make-chain-node "deepseek" "deepseek-v4-pro" "high")
              ) ;list
            ) ;make-chain-def
        (make-chain-def "coding-high-quality"
          "高质量"
          (list (make-chain-node "kimi-coding" "k3.1-pro" "high")
            (make-chain-node "zai-coding-cn" "glm-5.3" "high")
          ) ;list
        ) ;make-chain-def
      ) ;list
    ) ;define

    ;; 默认全局调度参数：continuous=#t、最长等待 1 小时、
    ;; 冷却/重试间隔 60 秒、单节点探活超时 8 秒。
    (define (make-default-all-in-one-config)
      (make-all-in-one-cfg #t 3600000 60000 8000 (all-in-one-default-chains))
    ) ;define

  ) ;begin
) ;define-library
