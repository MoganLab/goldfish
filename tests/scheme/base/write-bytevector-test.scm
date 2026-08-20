(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; write-bytevector
;; 向输出端口写入 bytevector。
;;
;; 语法
;; ----
;; (write-bytevector bv [port [start [end]]])

;; 写入整个 bytevector
(let ((p (open-output-bytevector)))
  (write-bytevector (bytevector 1 2 3) p)
  (check (get-output-bytevector p) => (bytevector 1 2 3))
) ;let

;; 带 start/end 范围
(let ((p (open-output-bytevector)))
  (write-bytevector (bytevector 1 2 3 4 5) p 1 4)
  (check (get-output-bytevector p) => (bytevector 2 3 4))
) ;let

(check-report)
