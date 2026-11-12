;; test-cache-touch!
;; 写入缓存条目（创建空文件，按需创建父目录）
;;
;; 语法
;; ----
;; (test-cache-touch! path)
;;
;; 参数
;; ----
;; path : string?，缓存文件路径
;;
;; 返回值
;; ------
;; unspecified

(import (scheme base) (scheme file) (liii check) (liii goldtest-cache))

(check-set-mode! 'report-failed)

;; ===== 场景1: touch 后文件存在且可重复 touch =====
(define probe (test-cache-path "00ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff02"))
(check-false (file-exists? probe))
(test-cache-touch! probe)
(check-true (file-exists? probe))
(test-cache-touch! probe)
(check-true (file-exists? probe))
(delete-file probe)
(check-false (file-exists? probe))

;; ===== 场景2: 是过程 =====
(check-true (procedure? test-cache-touch!))

(check-report)
