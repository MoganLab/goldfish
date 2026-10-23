(import (liii check))
(import (liii goldfmt format writer) (liii goldfmt record) (liii goldfmt scan))

(check-set-mode! 'report-failed)

;; emit-atom!
;; 将 atom 节点写入 writer，返回带定位信息的新 atom。
;;
;; 语法
;; ----
;; (emit-atom! node writer column)
;;
;; 参数
;; ----
;; node : atom?
;; writer : make-writer 的结果
;; column : integer?
;;
;; 返回值
;; ------
;; atom?
;; 含 indent/left-line/right-line 的新节点。

;; 测试：写入 atom 文本并返回定位节点
(let* ((w (make-writer 0))
       (new-node (emit-atom! (make-atom :value 42) w 0)))
  (check (writer-result w) => "42")
  (check (atom-value new-node) => 42)
  (check (atom-indent new-node) => 0)
) ;let

;; 测试：写入 symbol
(let* ((w (make-writer 2))
       (new-node (emit-atom! (make-atom :value 'abc) w 2)))
  (check (writer-result w) => "abc")
  (check (atom-left-line new-node) => 1)
  (check (atom-right-line new-node) => 1)
) ;let

(check-report)
