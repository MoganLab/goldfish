;; port-open?
;; 检查端口是否处于打开状态（可执行输入或输出）。
;;
;;
;; 语法
;; ----
;; (port-open? port)
;;
;; 参数
;; ----
;; port : port?
;; 要检查的端口。
;;
;; 返回值
;; ----
;; boolean?
;; 如果端口是打开的返回 #t，否则返回 #f。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 测试文本输入端口
(let ((p (open-input-string "hello")))
  (check (port-open? p) => #t)
  (close-port p)
  (check (port-open? p) => #f)
) ;let

;; 测试文本输出端口
(let ((p (open-output-string)))
  (check (port-open? p) => #t)
  (close-port p)
  (check (port-open? p) => #f)
) ;let

;; 测试使用 close-input-port 关闭输入端口
(let ((p (open-input-string "test")))
  (check (port-open? p) => #t)
  (close-input-port p)
  (check (port-open? p) => #f)
) ;let

;; 测试使用 close-output-port 关闭输出端口
(let ((p (open-output-string)))
  (check (port-open? p) => #t)
  (close-output-port p)
  (check (port-open? p) => #f)
) ;let

;; 测试二进制输入端口
(let ((p (open-input-bytevector #u8(1 2 3))))
  (check (port-open? p) => #t)
  (close-port p)
  (check (port-open? p) => #f)
) ;let

;; 测试二进制输出端口
(let ((p (open-output-bytevector)))
  (check (port-open? p) => #t)
  (close-port p)
  (check (port-open? p) => #f)
) ;let

;; 测试标准流端口均为打开状态
(check (port-open? (current-input-port)) => #t)
(check (port-open? (current-output-port)) => #t)
(check (port-open? (current-error-port)) => #t)

;; 参数检查：非端口参数抛出 type-error
(check-catch 'type-error (port-open? 123))
(check-catch 'type-error (port-open? "abc"))
(check-catch 'type-error (port-open? '()))

;; 参数个数检查
(check-catch 'wrong-number-of-args (port-open?))
(check-catch 'wrong-number-of-args
  (let ((p (open-input-string "")))
    (port-open? p p)
  ) ;let
) ;check-catch

(check-report)
