;; cached-count-reset!
;; 缓存命中计数清零
;;
;; 语法
;; ----
;; (cached-count-reset!)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; unspecified

(import (scheme base) (liii check) (liii goldtest-runner-report))

(check-set-mode! 'report-failed)

;; ===== 场景1: bump 后 reset，summary 输出不含 cached 计数 =====
(cached-count-bump!)
(cached-count-reset!)
(check (display-summary '()) => 0)

;; ===== 场景2: 是过程 =====
(check-true (procedure? cached-count-reset!))

(check-report)
