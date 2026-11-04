;; atom-left-line
;; 获取 atom 节点起始文本所在的行号。
;;
;; 语法
;; ----
;; (atom-left-line atom)
;;
;; 参数
;; ----
;; atom : atom?
;; 目标 atom 节点。
;;
;; 返回值
;; ----
;; integer?
;; 起始文本所在行号，0 表示行号未确定。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(let ((a (make-atom :depth 1 :indent 2 :left-line 3 :right-line 4 :value "x")))
  (check (atom-left-line a) => 3)
) ;let

(check-report)
