(import (scheme base)
  (liii check)
  (liii pi code core session-stats)
) ;import

(check-set-mode! 'report-failed)

;; pi-calculate-cache-hit-rate
;; 计算单轮请求或会话 token 统计的缓存命中率百分比。
;;
;; 语法
;; ----
;; (pi-calculate-cache-hit-rate input cache-read cache-write)
;; (pi-calculate-cache-hit-rate usage-or-tokens)
;;
;; 参数
;; ----
;; input : integer | usage-or-tokens
;; 未缓存输入 token 数，或包含 input/cacheRead/cacheWrite 的 alist/record。
;; cache-read : integer (可选)
;; 读缓存命中 token 数。
;; cache-write : integer (可选)
;; 写入缓存 token 数。
;;
;; 返回值
;; ----
;; number | #f
;; 命中率百分比（0.0 ~ 100.0）；若无缓存活动（cacheRead 与 cacheWrite 均为 0）或无 prompt 则返回 #f。

;; 完全命中（100%）：input=0, cache-read=1000, cache-write=0
(check (pi-calculate-cache-hit-rate 0 1000 0) => 100.0)

;; 部分命中：input=200, cache-read=800, cache-write=0 -> prompt=1000, 80%
(check (pi-calculate-cache-hit-rate 200 800 0) => 80.0)

;; 包含写缓存：input=100, cache-read=800, cache-write=100 -> prompt=1000, 80%
(check (pi-calculate-cache-hit-rate 100 800 100) => 80.0)

;; 首次请求写入缓存（无读缓存）：input=1000, cache-read=0, cache-write=500 -> 0.0%
(check (pi-calculate-cache-hit-rate 1000 0 500) => 0.0)

;; 无任何缓存活动（首轮无缓存支持）：返回 #f（不应展示 CH0.0%）
(check (pi-calculate-cache-hit-rate 1000 0 0) => #f)

;; 空 prompt / 零除安全保护：返回 #f
(check (pi-calculate-cache-hit-rate 0 0 0) => #f)

;; 支持传入 usage alist
(check (pi-calculate-cache-hit-rate '((input . 100) (cacheRead . 900) (cacheWrite . 0))) => 90.0)
(check (pi-calculate-cache-hit-rate '((input . 500) (cacheRead . 0) (cacheWrite . 0))) => #f)

;; 支持传入 pi-session-token-stats record
(check (pi-calculate-cache-hit-rate (make-pi-session-token-stats 50 100 950 0)) => 95.0)

(check-report)
