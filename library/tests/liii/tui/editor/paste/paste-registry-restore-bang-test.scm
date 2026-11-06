;; paste-registry-restore!
;; 从快照中恢复粘贴注册表的状态。
;;
;; 语法
;; ----
;; (paste-registry-restore! reg snapshot)
;;
;; 参数
;; ----
;; reg : paste-registry?
;; 粘贴注册表实例。
;;
;; snapshot : any
;; 此前由 paste-registry-snapshot 捕获的快照对象。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor paste))

(check-set-mode! 'report-failed)

(let* ((reg (make-paste-registry))
       (snap0 (paste-registry-snapshot reg))
       (m (paste-registry-add! reg "test"))
      ) ;
  (check (paste-registry-count reg) => 1)
  (paste-registry-restore! reg snap0)
  (check (paste-registry-count reg) => 0)
) ;let*

(check-report)
