(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; read-u8
;; 从输入端口读取一个字节。
;;
;; 语法
;; ----
;; (read-u8 [port])

(let ((p (open-input-bytevector (bytevector 65 66 67))))
  (check (read-u8 p) => 65)
  (check (read-u8 p) => 66)
  (check (read-u8 p) => 67)
) ;let

;; 文件结束返回 eof-object
(let ((p (open-input-bytevector (bytevector))))
  (check (eof-object? (read-u8 p)) => #t)
) ;let

;; 空字节参数端口
(let ((p (open-input-bytevector (bytevector 0 255))))
  (check (read-u8 p) => 0)
  (check (read-u8 p) => 255)
) ;let

(check-report)
