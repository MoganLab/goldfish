(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; close-port
;; 关闭输入或输出端口。
;;
;; 语法
;; ----
;; (close-port port)
;;
;; 参数
;; ----
;; port : port?
;; 要关闭的端口。
;;
;; 返回值
;; ------
;; 未指定。
;;
;; 副作用
;; ------
;; 关闭端口。
;; 测试关闭输入端口
(let ((p (open-input-string "hello")))
  (check (input-port-open? p) => #t)
  (close-port p)
  (check (input-port-open? p) => #f)
) ;let
;; 测试关闭输出端口
(let ((p (open-output-string)))
  (check (output-port-open? p) => #t)
  (close-port p)
  (check (output-port-open? p) => #f)
) ;let
;; 测试 binary-port? 和 textual-port?
(let ((p (open-input-string "test")))
  (check (binary-port? p) => #t)
  (check (textual-port? p) => #t)
  (close-port p)
) ;let
(check-report)
