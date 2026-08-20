(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; peek-u8
;; 读取下一个字节但不消耗它。
;;
;; 语法
;; ----
;; (peek-u8 [port])

(let ((p (open-input-bytevector (bytevector 65 66))))
  (check (peek-u8 p) => 65)
  (check (peek-u8 p) => 65)
  (check (read-u8 p) => 65)
  (check (peek-u8 p) => 66)
) ;let

;; 文件结束返回 eof-object
(let ((p (open-input-bytevector (bytevector))))
  (check (eof-object? (peek-u8 p)) => #t)
) ;let

(check-report)
