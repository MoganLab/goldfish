(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; output-port-open?
;; 检查输出端口是否处于打开状态。
;;
;; 语法
;; ----
;; (output-port-open? port)
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
;; 测试新创建的输出端口
(let ((p (open-output-string)))
  (check (output-port-open? p) => #t)
  (close-port p)
  (check (output-port-open? p) => #f)
) ;let
;; 测试使用 close-output-port 关闭
(let ((p (open-output-string)))
  (check (output-port-open? p) => #t)
  (display "test" p)
  (close-output-port p)
  (check (output-port-open? p) => #f)
) ;let
;; 测试输入端口不适用的情况
(let ((p (open-input-string "test")))
  (check (input-port-open? p) => #t)
  (close-port p)
  (check (input-port-open? p) => #f)
) ;let
(check-report)
