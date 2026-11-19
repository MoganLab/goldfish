;; make-paste-registry
;; 创建大段粘贴注册表实例，用于管理大段粘贴折叠与展开。
;;
;; 语法
;; ----
;; (make-paste-registry)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; paste-registry?
;; 粘贴注册表实例。

(import (scheme base) (liii check) (liii tui editor paste))

(check-set-mode! 'report-failed)

(let ((reg (make-paste-registry)))
  (check (paste-registry-count reg) => 0)
) ;let

(check-report)
