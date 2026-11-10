;; u8-ready?
;; 检查二进制输入端口是否有字节可读。
;;
;; 语法
;; ----
;; (u8-ready?)
;; (u8-ready? port)
;;
;; 参数
;; ----
;; port : input-port?
;; 可选，默认为当前输入端口。
;;
;; 返回值
;; ----
;; boolean?
;; 如果有字节可读则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 用于非阻塞输入检查
;; 2. 到达 EOF 时返回 #t（后续 read-u8 将返回 EOF 对象）

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(let ((p (open-input-bytevector (bytevector 10))))
  (check (u8-ready? p) => #t)
  (read-u8 p)
  (check (u8-ready? p) => #t)
  (close-port p)
) ;let

(check-report)
