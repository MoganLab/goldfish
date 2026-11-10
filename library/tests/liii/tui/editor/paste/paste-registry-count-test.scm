;; paste-registry-count
;; 获取粘贴注册表中当前登记的项目数量。
;;
;; 语法
;; ----
;; (paste-registry-count reg)
;;
;; 参数
;; ----
;; reg : paste-registry?
;; 粘贴注册表实例。
;;
;; 返回值
;; ----
;; integer?
;; 登记的项目总数。

(import (scheme base) (liii check) (liii tui editor paste))

(check-set-mode! 'report-failed)

(let ((reg (make-paste-registry)))
  (check (paste-registry-count reg) => 0)
  (paste-registry-add! reg "a")
  (check (paste-registry-count reg) => 1)
) ;let

(check-report)
