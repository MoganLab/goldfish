;; atom-depth
;; 获取 atom 节点的深度。
;;
;; 语法
;; ----
;; (atom-depth atom)
;;
;; 参数
;; ----
;; atom : atom?
;; 目标 atom 节点。
;;
;; 返回值
;; ----
;; integer?
;; 节点的深度。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(let ((a (make-atom :depth 1 :indent 2 :left-line 3 :right-line 4 :value "x")))
  (check (atom-depth a) => 1)
) ;let

(check-report)
