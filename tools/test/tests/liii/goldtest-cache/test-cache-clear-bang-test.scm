;; test-cache-clear!
;; 清空测试缓存目录
;;
;; 语法
;; ----
;; (test-cache-clear!)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; unspecified

(import (scheme base) (scheme file) (liii check) (liii goldtest-cache))

(check-set-mode! 'report-failed)

;; ===== 场景1: 清空后目录不存在，且重复清空不抛错 =====
(test-cache-clear!)
(check-false (file-exists? (test-cache-base-dir)))
(test-cache-clear!)
(check-false (file-exists? (test-cache-base-dir)))

;; ===== 场景2: 清空后 touch 重建正常 =====
(define probe (test-cache-path "00ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff03"))
(test-cache-touch! probe)
(check-true (file-exists? probe))
(test-cache-clear!)
(check-false (file-exists? probe))

;; ===== 场景3: 是过程 =====
(check-true (procedure? test-cache-clear!))

(check-report)
