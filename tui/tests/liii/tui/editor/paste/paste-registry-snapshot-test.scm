;; paste-registry-snapshot
;; 创建粘贴注册表当前状态的快照对象（用于撤销）。
;;
;; 语法
;; ----
;; (paste-registry-snapshot reg)
;;
;; 参数
;; ----
;; reg : paste-registry?
;; 粘贴注册表实例。
;;
;; 返回值
;; ----
;; any
;; 状态快照对象。

(import (scheme base) (liii check) (liii tui editor paste))

(check-set-mode! 'report-failed)

(let* ((reg (make-paste-registry))
       (m (paste-registry-add! reg "test"))
       (snap (paste-registry-snapshot reg))
      ) ;
  (check-true (not (not snap)))
) ;let*

(check-report)
