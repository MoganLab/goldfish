(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; open-output-bytevector
;; 创建用于收集字节的输出端口。
;;
;; 语法
;; ----
;; (open-output-bytevector)

;; 基本用法
(let ((p (open-output-bytevector)))
  (check (output-port? p) => #t)
  (write-u8 65 p)
  (write-u8 66 p)
  (check (get-output-bytevector p) => (bytevector 65 66))
) ;let

;; 初始为空
(let ((p (open-output-bytevector)))
  (check (get-output-bytevector p) => (bytevector))
) ;let

(check-report)
