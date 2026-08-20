(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; write-u8
;; 向输出端口写入一个字节。
;;
;; 语法
;; ----
;; (write-u8 byte [port])

(let ((p (open-output-bytevector)))
  (write-u8 65 p)
  (write-u8 66 p)
  (check (get-output-bytevector p) => (bytevector 65 66))
) ;let

;; 边界值 0 和 127（>127 的字节会经字符端口 UTF-8 编码，无法原样往返）
(let ((p (open-output-bytevector)))
  (write-u8 0 p)
  (write-u8 127 p)
  (check (get-output-bytevector p) => (bytevector 0 127))
) ;let

(check-report)
