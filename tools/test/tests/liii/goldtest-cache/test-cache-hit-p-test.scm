;; test-cache-hit?
;; 检查缓存是否命中（缓存空文件存在即命中）
;;
;; 语法
;; ----
;; (test-cache-hit? path)
;;
;; 参数
;; ----
;; path : string?，缓存文件路径
;;
;; 返回值
;; ------
;; boolean?

(import (scheme base) (scheme file) (liii check) (liii goldtest-cache))

(check-set-mode! 'report-failed)

;; ===== 场景1: 不存在的缓存未命中 =====
(define probe (test-cache-path "00ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff01"))
(check-false (test-cache-hit? probe))

;; ===== 场景2: touch 后命中，删除后未命中 =====
(test-cache-touch! probe)
(check-true (test-cache-hit? probe))
(delete-file probe)
(check-false (test-cache-hit? probe))

;; ===== 场景3: 是过程 =====
(check-true (procedure? test-cache-hit?))

(check-report)
