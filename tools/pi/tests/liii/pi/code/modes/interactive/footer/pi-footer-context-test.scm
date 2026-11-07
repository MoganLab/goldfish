;; pi-footer-set-token-stats!
;; 上下文占用显示：与 token 统计合并为页脚左下角统计串，
;; 并按占比阈值着色（>90% 红色、>70% 黄色，对齐上游 pi 页脚）
;;
;; 语法
;; ----
;; (pi-footer-set-token-stats! comp tokens latest-hit-rate context)
;;
;; 参数
;; ----
;; comp : component?
;; 底部状态栏组件实例。
;; tokens : pi-session-token-stats | alist
;; 会话累计 token 统计。
;; latest-hit-rate : number | #f
;; 最新单轮缓存命中率。
;; context : pi-context-usage | string | #f
;; 上下文占用：record 由页脚格式化并按占比着色；
;; 字符串直接作为上下文描述（不着色）；#f 表示沿用当前描述。
;;
;; 返回值
;; ----
;; unspecified
;; 无返回值。

(import (scheme base)
  (liii check)
  (liii string)
  (liii format base)
  (liii format color)
  (liii tui component)
  (liii pi code core session-stats)
  (liii pi code core context-usage)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report-failed)

;; srfi-166 的着色返回惰性串，需经 show 展开为普通字符串

(define (red s)
  (show #f (as-red s))
) ;define

(define (yellow s)
  (show #f (as-yellow s))
) ;define

;; 1. 上下文 record 与 token 统计合并为左下角统计串
(let ((ft (make-pi-footer)) (tokens (make-pi-session-token-stats 100 50 2000 0)))
  (pi-footer-set-token-stats! ft
    tokens
    95.2
    (make-pi-context-usage 12.3 1048576 #t)
  ) ;pi-footer-set-token-stats!
  (check (pi-footer-stats ft) => "↑100 ↓50 R2.0k CH95.2% 12.3%/1.0M (auto)")
) ;let

;; 2. 压缩后尚无新响应：占比未知显示 "?"
(let ((ft (make-pi-footer)) (tokens (make-pi-session-token-stats 100 50 2000 0)))
  (pi-footer-set-token-stats! ft
    tokens
    95.2
    (make-pi-context-usage #f 1048576 #f)
  ) ;pi-footer-set-token-stats!
  (check (pi-footer-stats ft) => "↑100 ↓50 R2.0k CH95.2% ?/1.0M (auto)")
) ;let

;; 3. 占比超过 90%：上下文段标红
(let* ((ft (make-pi-footer)) (tokens (make-pi-session-token-stats 100 50 2000 0)))
  (pi-footer-set-token-stats! ft
    tokens
    95.2
    (make-pi-context-usage 95.0 1048576 #t)
  ) ;pi-footer-set-token-stats!
  (let ((line2 (cadr (component-render ft 80))))
    (check-true (string-contains line2 (red "95.0%/1.0M (auto)")))
    (check-false (string-contains line2 (yellow "95.0%/1.0M (auto)")))
  ) ;let
) ;let*

;; 4. 占比 70%~90%：上下文段标黄
(let* ((ft (make-pi-footer)) (tokens (make-pi-session-token-stats 100 50 2000 0)))
  (pi-footer-set-token-stats! ft
    tokens
    95.2
    (make-pi-context-usage 75.0 1048576 #t)
  ) ;pi-footer-set-token-stats!
  (let ((line2 (cadr (component-render ft 80))))
    (check-true (string-contains line2 (yellow "75.0%/1.0M (auto)")))
    (check-false (string-contains line2 (red "75.0%/1.0M (auto)")))
  ) ;let
) ;let*

;; 5. 占比低于 70%：不着色
(let* ((ft (make-pi-footer)) (tokens (make-pi-session-token-stats 100 50 2000 0)))
  (pi-footer-set-token-stats! ft
    tokens
    95.2
    (make-pi-context-usage 12.3 1048576 #t)
  ) ;pi-footer-set-token-stats!
  (let ((line2 (cadr (component-render ft 80))))
    (check-true (string-contains line2 "12.3%/1.0M (auto)"))
    (check-false (string-contains line2 (red "12.3%/1.0M (auto)")))
    (check-false (string-contains line2 (yellow "12.3%/1.0M (auto)")))
  ) ;let
) ;let*

;; 6. 未知占比切换后按记忆的窗口显示 "?"
(let* ((ft (make-pi-footer)) (tokens (make-pi-session-token-stats 100 50 2000 0)))
  (pi-footer-set-token-stats! ft
    tokens
    95.2
    (make-pi-context-usage 12.3 200000 #t)
  ) ;pi-footer-set-token-stats!
  (check (pi-footer-stats ft) => "↑100 ↓50 R2.0k CH95.2% 12.3%/200k (auto)")
  (pi-footer-set-context-unknown! ft)
  (check (pi-footer-stats ft) => "↑100 ↓50 R2.0k CH95.2% ?/200k (auto)")
) ;let*

;; 7. 上下文窗口缺失时沿用最近一次已知容量，不退化为 0
(let* ((ft (make-pi-footer :context-window 1048576))
       (tokens (make-pi-session-token-stats 100 50 2000 0))
      ) ;
  (pi-footer-set-token-stats! ft
    tokens
    95.2
    (make-pi-context-usage 12.3 1048576 #t)
  ) ;pi-footer-set-token-stats!
  (check (pi-footer-stats ft) => "↑100 ↓50 R2.0k CH95.2% 12.3%/1.0M (auto)")
  (pi-footer-set-token-stats! ft tokens 95.2 (make-pi-context-usage #f #f #t))
  (check (pi-footer-stats ft) => "↑100 ↓50 R2.0k CH95.2% 0.0%/1.0M (auto)")
  (check (pi-footer-context-window ft) => 1048576)
) ;let*

(check-report)
