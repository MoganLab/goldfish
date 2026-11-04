;; pi-session-token-stats-add-usage!
;; 将单次请求的 usage 统计累加到会话全局 token 统计 record 中。
;;
;; 语法
;; ----
;; (pi-session-token-stats-add-usage! tokens usage)
;;
;; 参数
;; ----
;; tokens : pi-session-token-stats
;; 会话全局 token 统计 record（就地修改）。
;; usage : alist
;; 单次请求返回的 usage alist。
;;
;; 返回值
;; ----
;; unspecified
;; 无返回值。

(import (scheme base)
  (liii check)
  (liii pi code core session-stats)
) ;import

(check-set-mode! 'report-failed)

;; 1. 基础累加：空 tokens 累加第一条 usage
(let ((tokens (make-pi-session-token-stats 0 0 0 0))
      (usage '((input . 100) (output . 40) (cacheRead . 800) (cacheWrite . 50))))
  (pi-session-token-stats-add-usage! tokens usage)
  (check (pi-session-token-stats-input tokens) => 100)
  (check (pi-session-token-stats-output tokens) => 40)
  (check (pi-session-token-stats-cache-read tokens) => 800)
  (check (pi-session-token-stats-cache-write tokens) => 50)
  (check (pi-session-token-stats-total tokens) => 990)

  ;; 2. 多轮累加：第二条 usage 继续累加
  (let ((usage2 '(("input" . 200) ("output" . 60) ("cacheRead" . 900) ("cacheWrite" . 0))))
    (pi-session-token-stats-add-usage! tokens usage2)
    (check (pi-session-token-stats-input tokens) => 300)
    (check (pi-session-token-stats-output tokens) => 100)
    (check (pi-session-token-stats-cache-read tokens) => 1700)
    (check (pi-session-token-stats-cache-write tokens) => 50)
    (check (pi-session-token-stats-total tokens) => 2150)))

;; 3. 缺失字段默认补 0
(let ((tokens (make-pi-session-token-stats 10 20 30 40))
      (usage '()))
  (pi-session-token-stats-add-usage! tokens usage)
  (check (pi-session-token-stats-input tokens) => 10)
  (check (pi-session-token-stats-output tokens) => 20)
  (check (pi-session-token-stats-cache-read tokens) => 30)
  (check (pi-session-token-stats-cache-write tokens) => 40))

(check-report)
