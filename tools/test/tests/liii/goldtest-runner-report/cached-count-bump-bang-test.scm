;; cached-count-bump!
;; 缓存命中计数自增
;;
;; 语法
;; ----
;; (cached-count-bump!)
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

;; ===== 场景1: reset 后连续 bump，display-summary 输出含计数 =====
(cached-count-reset!)
(cached-count-bump!)
(cached-count-bump!)
(cached-count-bump!)
;; 通过 display-summary 的返回值（失败数）间接验证不报错
(check (display-summary (list (cons "a.scm" 0) (cons "b.scm" 0) (cons "c.scm" 0))) => 0)
(cached-count-reset!)

;; ===== 场景2: 是过程 =====
(check-true (procedure? cached-count-bump!))

(check-report)
