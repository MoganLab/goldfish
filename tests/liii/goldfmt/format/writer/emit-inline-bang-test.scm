(import (liii check))
(import (liii goldfmt format writer) (liii goldfmt record) (liii goldfmt scan))

(check-set-mode! 'report-failed)

;; emit-inline!
;; 将可内联节点写入 writer，返回带定位信息的新节点。
;;
;; 语法
;; ----
;; (emit-inline! node writer column)
;;
;; 返回值
;; ------
;; atom? 或 env?

;; 测试：atom 节点内联输出
(let* ((w (make-writer 0))
       (new-node (emit-inline! (make-atom :value 'x) w 0)))
  (check (writer-result w) => "x")
  (check (atom? new-node) => #t)
) ;let

;; 测试：短 env 内联输出为单行括号形式
(let* ((w (make-writer 0))
       (env (scan '(+ 1 2)))
       (new-node (emit-inline! env w 0)))
  (check (writer-result w) => "(+ 1 2)")
  (check (env? new-node) => #t)
) ;let

;; 测试：嵌套 env 内联输出
(let* ((w (make-writer 0))
       (env (scan '(+ 1 (* 2 3))))
       (new-node (emit-inline! env w 0)))
  (check (writer-result w) => "(+ 1 (* 2 3))")
) ;let

(check-report)
