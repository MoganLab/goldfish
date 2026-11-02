;; pi-builtin-augment-all
;; 把全部内置 provider 缺失的模型合并/补齐进 models-store。
;;
;; 语法
;; ----
;; (pi-builtin-augment-all models-store)
;;
;; 参数
;; ----
;; models-store : list?
;; 当前的模型元数据存储关联列表。
;;
;; 返回值
;; ----
;; list?
;; 补齐全部内置模型后的元数据存储列表。

(import (liii check) (liii pi ai models builtin))

(check-set-mode! 'report-failed)

(let* ((empty-store '()) (augmented (pi-builtin-augment-all empty-store)))
  ;; 应该包含 6 个内置 provider（anthropic 已移除）
  (check (length augmented) => 5)
  (check-false (pair? (assoc "anthropic" augmented)))
  (check-true (pair? (assoc "deepseek" augmented)))
  (check-true (pair? (assoc "google" augmented)))
  (check-true (pair? (assoc "kimi-coding" augmented)))
  (check-true (pair? (assoc "minimax-cn" augmented)))
  (check-true (pair? (assoc "minimax-cn" augmented)))
  (check-true (pair? (assoc "zai-coding-cn" augmented)))
) ;let*

;; 如果已有部分 provider 模型，不应覆盖已有同 id 模型，但应补齐缺失模型
(let* ((partial-store
         (list
           (cons "deepseek"
             (list
               (cons "models"
                 (list (list (cons "id" "deepseek-v4-pro") (cons "name" "Custom DeepSeek V4 Pro"))
                 ) ;list
               ) ;cons
             ) ;list
           ) ;cons
         ) ;list
       ) ;partial-store
       (augmented (pi-builtin-augment-all partial-store))
       (ds-entry (assoc "deepseek" augmented))
       (ds-models (cdr (assoc "models" (cdr ds-entry))))
      ) ;
  ;; deepseek 原有 1 个，补齐缺失的 2 个，共 3 个
  (check (length ds-models) => 3)
  ;; 原有的 custom 应该保持在第一项且名字不变
  (check (cdr (assoc "id" (car ds-models))) => "deepseek-v4-pro")
  (check (cdr (assoc "name" (car ds-models))) => "Custom DeepSeek V4 Pro")
) ;let*

(check-report)
