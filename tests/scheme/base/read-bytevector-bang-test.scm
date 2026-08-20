(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; read-bytevector!
;; 将字节读入现有 bytevector。
;;
;; 语法
;; ----
;; (read-bytevector! bv [port [start [end]]])

;; 读取填充整个 bytevector
(let ((bv (make-bytevector 3)))
  (check (read-bytevector! bv (open-input-bytevector (bytevector 65 66 67)))
         => 3)
  (check bv => (bytevector 65 66 67))
) ;let

;; 数据不足时返回实际读取的字节数
(let ((bv (make-bytevector 5)))
  (check (read-bytevector! bv (open-input-bytevector (bytevector 65 66)))
         => 2)
  (check bv => (bytevector 65 66 0 0 0))
) ;let

(check-report)
