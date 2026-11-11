;; all-in-one-chain->model
;; 将 <chain-def> 映射为标准模型元数据 alist。
;;
;; 语法
;; ----
;; (all-in-one-chain->model chain)
;;
;; 参数
;; ----
;; chain : <chain-def>
;; 链定义记录。
;;
;; 返回值
;; ----
;; alist
;; 包含 id, name, provider="all-in-one", api="all-in-one", reasoning=#t 的模型元数据。

(import (scheme base)
  (liii check)
  (liii pi all_in_one types)
  (liii pi all_in_one provider)
) ;import

(check-set-mode! 'report-failed)

(let* ((nodes (list (make-chain-node "kimi1" "kimi-k3" "high")))
       (c (make-chain-def "coding-deep" "深度流水线" nodes))
       (m (all-in-one-chain->model c))
      ) ;
  (check (cdr (assoc "id" m)) => "coding-deep")
  (check (cdr (assoc "name" m)) => "深度流水线")
  (check (cdr (assoc "provider" m)) => "all-in-one")
  (check (cdr (assoc "api" m)) => "all-in-one")
  (check (cdr (assoc "reasoning" m)) => #t)
) ;let*

(check-report)
