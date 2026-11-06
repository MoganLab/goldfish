;; atom-value
;; 获取 atom 节点保存的值。
;;
;; 语法
;; ----
;; (atom-value atom)
;;
;; 参数
;; ----
;; atom : atom?
;; 目标 atom 节点。
;;
;; 返回值
;; ----
;; any?
;; 节点保存的原始 datum 值。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(let ((a (make-atom :depth 1 :indent 2 :left-line 3 :right-line 4 :value "x")))
  (check (atom-value a) => "x")
) ;let

(check-report)
