;; test-cache-force?
;; 查询当前缓存强制绕过标志
;;
;; 语法
;; ----
;; (test-cache-force?)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; boolean?

(import (scheme base) (liii check) (liii goldtest-runner))

(check-set-mode! 'report-failed)

;; ===== 场景1: 与 setter 联动 =====
(set-test-cache-force! #f)
(check-false (test-cache-force?))
(set-test-cache-force! #t)
(check-true (test-cache-force?))

;; ===== 场景2: 是无参过程 =====
(check-true (procedure? test-cache-force?))

;; 还原默认状态，避免影响其他测试
(set-test-cache-force! #f)

(check-report)
