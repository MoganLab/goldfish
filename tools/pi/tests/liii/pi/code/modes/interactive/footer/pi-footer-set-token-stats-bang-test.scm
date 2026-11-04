;; pi-footer-set-token-stats!
;; 根据 Token 累计统计与最新命中率自动格式化并更新状态栏左下角统计。
;;
;; 语法
;; ----
;; (pi-footer-set-token-stats! comp tokens)
;; (pi-footer-set-token-stats! comp tokens latest-hit-rate)
;; (pi-footer-set-token-stats! comp tokens latest-hit-rate context-info)
;;
;; 参数
;; ----
;; comp : component?
;; 底部状态栏组件实例。
;; tokens : pi-session-token-stats | alist
;; 会话累计 token 统计。
;; latest-hit-rate : number | #f (可选)
;; 最新单轮缓存命中率。
;; context-info : string (可选，缺省为 "0.0%/1.0M (auto)")
;; 上下文窗口使用比例描述。
;;
;; 返回值
;; ----
;; unspecified
;; 无返回值。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui component)
  (liii pi code core session-stats)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report-failed)

;; 1. 命中缓存：左下角同时呈现 R 与 CHxx.x%
(let ((ft (make-pi-footer))
      (tokens (make-pi-session-token-stats 100 50 2000 0)))
  (pi-footer-set-token-stats! ft tokens 95.2)
  (check (pi-footer-stats ft) => "↑100 ↓50 R2.0k CH95.2% 0.0%/1.0M (auto)")
  (let ((line2 (cadr (component-render ft 80))))
    (check-true (string-contains line2 "CH95.2%"))))

;; 2. 无缓存活动：仅展示传输量，不展示 CH 字段
(let ((ft (make-pi-footer))
      (tokens (make-pi-session-token-stats 1000 50 0 0)))
  (pi-footer-set-token-stats! ft tokens #f)
  (check (pi-footer-stats ft) => "↑1.0k ↓50 0.0%/1.0M (auto)"))

;; 3. 全 0 初始状态：保留 context-info
(let ((ft (make-pi-footer))
      (tokens (make-pi-session-token-stats 0 0 0 0)))
  (pi-footer-set-token-stats! ft tokens #f)
  (check (pi-footer-stats ft) => "0.0%/1.0M (auto)"))

;; 4. 自定义上下文容量提示
(let ((ft (make-pi-footer))
      (tokens (make-pi-session-token-stats 200 40 5000 0)))
  (pi-footer-set-token-stats! ft tokens 96.0 "1.5%/200k (auto)")
  (check (pi-footer-stats ft) => "↑200 ↓40 R5.0k CH96.0% 1.5%/200k (auto)"))

(check-report)
