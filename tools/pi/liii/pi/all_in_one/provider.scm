;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi all_in_one provider)
  (import (scheme base)
    (liii list)
    (liii pi all_in_one types)
    (liii pi all_in_one loader)
  ) ;import
  (export all-in-one-chain->model all-in-one-chains->models all-in-one-models
    all-in-one-provider-available? all-in-one-provider-entry
  ) ;export
  (begin

    ;; all-in-one-chain->model
    ;; 将 <chain-def> 映射为标准模型元数据 alist。
    (define (all-in-one-chain->model chain)
      (list (cons "id" (chain-def-id chain))
        (cons "name" (chain-def-name chain))
        (cons "provider" "all-in-one")
        (cons "api" "all-in-one")
        (cons "reasoning" #t)
      ) ;list
    ) ;define

    ;; all-in-one-chains->models
    ;; 将 <chain-def> 列表映射为模型元数据 alist 列表。
    (define (all-in-one-chains->models chains)
      (map all-in-one-chain->model chains)
    ) ;define

    ;; all-in-one-models
    ;; 读取配置并发现伪模型列表。若未配置或非法返回空列表。
    (define (all-in-one-models . opt-config-dir)
      (let ((cfg (apply all-in-one-load-config opt-config-dir)))
        (if (and cfg (all-in-one-cfg? cfg))
          (all-in-one-chains->models (all-in-one-cfg-chains cfg))
          '()
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
