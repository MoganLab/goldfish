(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; open-input-bytevector
;; 创建读取 bytevector 内容的输入端口。
;;
;; 语法
;; ----
;; (open-input-bytevector bv)

(let ((p (open-input-bytevector (bytevector 65 66 67))))
  (check (input-port? p) => #t)
  (check (read-u8 p) => 65)
  (check (read-u8 p) => 66)
  (check (read-u8 p) => 67)
  (check (eof-object? (read-u8 p)) => #t)
) ;let

;; 空 bytevector
(let ((p (open-input-bytevector (bytevector))))
  (check (eof-object? (read-u8 p)) => #t)
) ;let

(check-report)
