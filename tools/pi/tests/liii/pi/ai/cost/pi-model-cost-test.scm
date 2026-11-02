;; pi-model-cost
;; 获取指定模型在特定服务商下的定价费率表。
;;
;; 语法
;; ----
;; (pi-model-cost provider model-id custom-models)
;;
;; 参数
;; ----
;; provider : string?
;; 服务商标识符。
;;
;; model-id : string?
;; 模型标识符。
;;
;; custom-models : list?
;; 自定义模型配置列表。
;;
;; 返回值
;; ----
;; alist
;; 包含 input、output、cacheRead、cacheWrite 等费率的关联列表。

(import (scheme base) (liii check) (liii pi ai cost))

(check-set-mode! 'report-failed)

;; 1. 查找已知模型定价 (Gemini 3.8 Flash)
(let ((c (pi-model-cost "google" "gemini-3.8-flash" '())))
  (check-true (pair? c))
  (check (cdr (assoc 'input c)) => 0.75)
  (check (cdr (assoc 'output c)) => 3.75)
  (check (cdr (assoc 'cacheRead c)) => 0.075)
  (check (cdr (assoc 'cacheWrite c)) => 0.0)
) ;let

;; 2. 查找已知模型定价 (Kimi k3)
(let ((c (pi-model-cost "kimi-coding" "k3" '())))
  (check-true (pair? c))
  (check (cdr (assoc 'input c)) => 3.0)
  (check (cdr (assoc 'output c)) => 15.0)
  (check (cdr (assoc 'cacheRead c)) => 0.3)
  (check (cdr (assoc 'cacheWrite c)) => 0.0)
) ;let

;; 3. 查找带有 tiers 的模型定价 (Gemini 2.5 Pro)
(let ((c (pi-model-cost "google" "gemini-2.5-pro" '())))
  (check-true (pair? c))
  (check (cdr (assoc 'input c)) => 1.25)
  (check (cdr (assoc 'output c)) => 10.0)
  (check (cdr (assoc 'cacheRead c)) => 0.125)
  (check (cdr (assoc 'cacheWrite c)) => 0.0)
  (check-true (pair? (cdr (assoc 'tiers c))))
) ;let

;; 4. 未知模型兜底归零
(let ((c (pi-model-cost "unknown-provider" "unknown-model" '())))
  (check-true (pair? c))
  (check (cdr (assoc 'input c)) => 0.0)
  (check (cdr (assoc 'output c)) => 0.0)
  (check (cdr (assoc 'cacheRead c)) => 0.0)
  (check (cdr (assoc 'cacheWrite c)) => 0.0)
) ;let

;; 5. MiniMax 系列定价（minimax-cn 回退到 minimax）
(let ((c (pi-model-cost "minimax-cn" "MiniMax-M3.1-Flash" '())))
  (check-true (pair? c))
  (check (cdr (assoc 'input c)) => 0.3)
  (check (cdr (assoc 'output c)) => 1.2)
  (check (cdr (assoc 'cacheRead c)) => 0.06)
) ;let

(let ((c (pi-model-cost "minimax-cn" "MiniMax-M2.7-highspeed" '())))
  (check-true (pair? c))
  (check (cdr (assoc 'input c)) => 0.6)
  (check (cdr (assoc 'output c)) => 2.4)
  (check (cdr (assoc 'cacheWrite c)) => 0.375)
) ;let

(let ((c (pi-model-cost "minimax-cn" "MiniMax-M3" '())))
  (check-true (pair? c))
  (check-true (pair? (cdr (assoc 'tiers c))))
) ;let

(check-report)
