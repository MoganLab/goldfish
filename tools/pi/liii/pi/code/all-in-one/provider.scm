;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;; all-in-one 元供应商的伪模型目录投影。

;; provider 只负责把配置里的链投影成模型目录条目：
;; 默认链与默认配置的唯一真值源在 defaults.scm，
;; 加载与缺省回退的统一入口在 loader.scm。
(define-library (liii pi code all-in-one provider)
  (import (scheme base)
    (liii list)
    (liii pi ai providers registry)
    (liii pi code all-in-one types)
    (liii pi code all-in-one loader)
  ) ;import

  ;; 模型目录
  (export all-in-one-models all-in-one-provider-entry)
  ;; 可用性
  (export all-in-one-provider-available?)
  ;; 链元数据
  (export chain-conservative-context-window)
  (begin

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

    ;; 保守窗口不变量：取链内所有候选节点 contextWindow 的最小值，避免压缩判定
    ;; 按最强节点高估容量。
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

    (define (%chain->model chain)
      (list (cons "id" (chain-def-id chain))
        (cons "name" (chain-def-name chain))
        (cons "provider" "all-in-one")
        (cons "api" "all-in-one")
        (cons "reasoning" #t)
        (cons "contextWindow" (chain-conservative-context-window chain))
        (cons "maxTokens" 65536)
      ) ;list
    ) ;define

    (define (all-in-one-models . opt-config-dir)
      (let ((cfg (apply all-in-one-load-or-default-config opt-config-dir)))
        (map %chain->model (all-in-one-cfg-chains cfg))
      ) ;let
    ) ;define

    ;; 可用性判定只表示“用户显式配置过 all-in-one”，不因默认回退而恒为真，
    ;; 因此不会被误当成内置 provider 反复出现在目录里。
    (define (all-in-one-provider-available? . opt-config-dir)
      (let ((cfg (apply all-in-one-load-config opt-config-dir)))
        (and cfg (all-in-one-cfg? cfg) (pair? (all-in-one-cfg-chains cfg)))
      ) ;let
    ) ;define

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
