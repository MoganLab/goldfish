;; paste-registry-clear!
;; 清空粘贴注册表中的全部记录。
;;
;; 语法
;; ----
;; (paste-registry-clear! reg)
;;
;; 参数
;; ----
;; reg : paste-registry?
;; 粘贴注册表实例。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor paste))

(check-set-mode! 'report-failed)

(let ((reg (make-paste-registry)))
  (paste-registry-add! reg "hello")
  (check (paste-registry-count reg) => 1)
  (paste-registry-clear! reg)
  (check (paste-registry-count reg) => 0)
) ;let

(check-report)
