;; positioned-atom
;; 为 atom 节点生成带缩进与行号定位信息的拷贝。
;;
;; 语法
;; ----
;; (positioned-atom node indent left-line right-line)
;;
;; 参数
;; ----
;; node : atom?
;; 原始 atom 节点。
;;
;; indent : integer?
;; 节点左侧起始列。
;;
;; left-line : integer?
;; 节点开始输出的行号。
;;
;; right-line : integer?
;; 节点完成输出的行号。
;;
;; 返回值
;; ----
;; atom?
;; 带定位信息的新 atom 节点。
;;
;; 说明
;; ----
;; 原始节点的 depth 与 value 会被保留。

(import (liii check) (liii goldfmt format writer) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; 测试：保留 value 并带定位信息
(let* ((node (make-atom :value 7)) (p (positioned-atom node 4 1 3)))
  (check (atom-value p) => 7)
  (check (atom-indent p) => 4)
  (check (atom-left-line p) => 1)
  (check (atom-right-line p) => 3)
) ;let*

;; 测试：depth 被保留
(let* ((node (make-atom :depth 2 :value 'x)) (p (positioned-atom node 0 1 1)))
  (check (atom-depth p) => 2)
  (check (atom-value p) => 'x)
) ;let*

(check-report)
