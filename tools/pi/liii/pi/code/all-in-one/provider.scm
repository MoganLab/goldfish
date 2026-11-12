;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code all-in-one provider)
  (import (scheme base)
    (liii list)
    (liii pi ai providers registry)
    (liii pi code all-in-one types)
    (liii pi code all-in-one loader)
  ) ;import
  (export all-in-one-chain->model all-in-one-chains->models all-in-one-models
    all-in-one-provider-available? all-in-one-provider-entry
    all-in-one-default-chains chain-conservative-context-window
  ) ;export
  (begin

    ;; all-in-one-default-chains
    ;; 默认双链路定义：
    ;; 1. 快速推进链路：google -> minimax-cn -> deepseek
    ;; 2. 高质攻坚链路：kimi-coding -> zai-coding-cn
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

    ;; 查找节点在内置模型目录中的上下文窗口
    (define (%node-builtin-context-window provider model)
      (let* ((p-models (cdr (or (assoc provider PI_PROVIDER_BUILTIN_MODELS) '(#f))))
             (m
               (find
                 (lambda (entry) (equal? (cdr (or (assoc "id" entry) '(#f . #f))) model))
                 p-models
               ) ;find
             ) ;m
            ) ;
        (if m (cdr (or (assoc "contextWindow" m) '(#f . 1000000))) 1000000)
      ) ;let*
    ) ;define

    ;; chain-conservative-context-window
    ;; 计算链路的保守上下文窗口（所有节点中 contextWindow 的最小值），作为安全兜底
    (define (chain-conservative-context-window chain)
      (let ((nodes (chain-def-nodes chain)))
        (if (not (pair? nodes))
          1000000
          (apply min
            (map (lambda (n)
                   (%node-builtin-context-window (chain-node-provider n) (chain-node-model n))
                 ) ;lambda
              nodes
            ) ;map
          ) ;apply
        ) ;if
      ) ;let
    ) ;define

    ;; all-in-one-chain->model
    ;; 将 <chain-def> 映射为标准模型元数据 alist。
    (define (all-in-one-chain->model chain)
      (list (cons "id" (chain-def-id chain))
        (cons "name" (chain-def-name chain))
        (cons "provider" "all-in-one")
        (cons "api" "all-in-one")
        (cons "reasoning" #t)
        (cons "contextWindow" (chain-conservative-context-window chain))
        (cons "maxTokens" 65536)
      ) ;list
    ) ;define

    ;; all-in-one-chains->models
    ;; 将 <chain-def> 列表映射为模型元数据 alist 列表。
    (define (all-in-one-chains->models chains)
      (map all-in-one-chain->model chains)
    ) ;define

    ;; all-in-one-models
    ;; 读取配置并发现伪模型列表。若未配置或非法回退为默认伪模型列表。
    (define (all-in-one-models . opt-config-dir)
      (let ((cfg (apply all-in-one-load-config opt-config-dir)))
        (if (and cfg (all-in-one-cfg? cfg) (pair? (all-in-one-cfg-chains cfg)))
          (all-in-one-chains->models (all-in-one-cfg-chains cfg))
          (all-in-one-chains->models (all-in-one-default-chains))
        ) ;if
      ) ;let
    ) ;define

    ;; all-in-one-provider-available?
    ;; 检查元供应商在给定配置目录下是否有效可用（存在有效配置且至少配置了一条链）。
    (define (all-in-one-provider-available? . opt-config-dir)
      (let ((cfg (apply all-in-one-load-config opt-config-dir)))
        (and cfg (all-in-one-cfg? cfg) (pair? (all-in-one-cfg-chains cfg)))
      ) ;let
    ) ;define

    ;; all-in-one-provider-entry
    ;; 构造元供应商目录条目 ("all-in-one" ("name" . "all-in-one") ("api" . "all-in-one") ("models" ...))
    (define (all-in-one-provider-entry . opt-config-dir)
      (cons "all-in-one"
        (list (cons "name" "all-in-one")
          (cons "api" "all-in-one")
          (cons "models" (apply all-in-one-models opt-config-dir))
        ) ;list
      ) ;cons
    ) ;define

  ) ;begin
) ;define-library
