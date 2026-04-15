(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; input-port-open?
;; 检查输入端口是否处于打开状态。
;;
;; 语法
;; ----
;; (input-port-open? port)
;;
;; 参数
;; ----
;; port : port?
;; 要检查的端口。
;;
;; 返回值
;; ------
;; boolean?
;; 如果端口是打开的返回 #t，否则返回 #f。
;; 测试新打开的输入端口
(let ((p (open-input-string "hello")))
  (check (input-port-open? p) => #t)
  (close-port p)
  (check (input-port-open? p) => #f)
) ;let
;; 测试关闭后重新检查
(let ((p (open-input-string "test")))
  (check (input-port-open? p) => #t)
  (close-input-port p)
  (check (input-port-open? p) => #f)
) ;let
;; 测试输出端口（应该也适用）
(let ((p (open-output-string)))
  (check (output-port-open? p) => #t)
  (close-port p)
  (check (output-port-open? p) => #f)
) ;let
(check-report)
