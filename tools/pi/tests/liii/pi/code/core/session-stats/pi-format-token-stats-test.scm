;; pi-format-token-stats
;; 构建供 Footer 或终端状态栏展示的紧凑 Token 传输与缓存统计行。
;;
;; 语法
;; ----
;; (pi-format-token-stats tokens)
;; (pi-format-token-stats tokens latest-hit-rate)
;;
;; 参数
;; ----
;; tokens : pi-session-token-stats | alist
;; 会话累计 token 统计。
;; latest-hit-rate : number | #f (可选)
;; 最新单轮缓存命中率。
;;
;; 返回值
;; ----
;; string
;; 紧凑统计字符串，形如「↑1.2k ↓45 R15k CH98.5%」。

;; 1. 首轮无缓存：仅展示传输量，不展示 CH 字段

(import (scheme base) (liii check) (liii pi code core session-stats))

(check-set-mode! 'report-failed)

(let ((tokens (make-pi-session-token-stats 1200 45 0 0)))
  (check (pi-format-token-stats tokens) => "↑1.2k ↓45")
  (check (pi-format-token-stats tokens #f) => "↑1.2k ↓45")
) ;let

;; 2. 多轮命中缓存：展示 R 与最新 CHxx.x%
(let ((tokens (make-pi-session-token-stats 300 250 15200 0)))
  (check (pi-format-token-stats tokens 98.06) => "↑300 ↓250 R15k CH98.1%")
) ;let

;; 3. 包含写缓存：展示 W 字段
(let ((tokens (make-pi-session-token-stats 100 50 1200 300)))
  (check (pi-format-token-stats tokens 80.0) => "↑100 ↓50 R1.2k W300 CH80.0%")
) ;let

;; 4. 支持 alist 格式输入
(let ((tokens '((input . 500) (output . 80) (cacheRead . 5000) (cacheWrite . 0))))
  (check (pi-format-token-stats tokens 90.9) => "↑500 ↓80 R5.0k CH90.9%")
) ;let

;; 5. 全 0 边界：返回空串
(let ((tokens (make-pi-session-token-stats 0 0 0 0)))
  (check (pi-format-token-stats tokens) => "")
) ;let

(check-report)
