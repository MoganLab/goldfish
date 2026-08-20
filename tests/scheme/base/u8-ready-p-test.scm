(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; u8-ready?
;; 判断输入端口是否已有可读的字节。
;;
;; 语法
;; ----
;; (u8-ready? [port])
;;
;; 说明
;; ----
;; goldfish 不区分文本/二进制端口，以 char-ready? 实现。

(check (procedure? u8-ready?) => #t)

(let ((p (open-input-bytevector (bytevector 65))))
  (check (u8-ready? p) => #t)
  (check (read-u8 p) => 65)
) ;let

;; 无参时使用当前输入端口
(check (procedure? (u8-ready?)) => #f)

(check-report)
