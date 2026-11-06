;; atom-indent
;; 获取 atom 节点的缩进量。
;;
;; 语法
;; ----
;; (atom-indent atom)
;;
;; 参数
;; ----
;; atom : atom?
;; 目标 atom 节点。
;;
;; 返回值
;; ----
;; integer?
;; 节点的缩进量，-1 表示缩进尚未确认。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(let ((a (make-atom :depth 1 :indent 2 :left-line 3 :right-line 4 :value "x")))
  (check (atom-indent a) => 2)
) ;let

(check-report)
