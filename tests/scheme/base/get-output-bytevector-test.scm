(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; get-output-bytevector
;; 返回输出 bytevector 端口中已写入的字节。
;;
;; 语法
;; ----
;; (get-output-bytevector port)

(let ((p (open-output-bytevector)))
  (write-bytevector (bytevector 1 2 3) p)
  (check (get-output-bytevector p) => (bytevector 1 2 3))
) ;let

;; 空端口
(let ((p (open-output-bytevector)))
  (check (get-output-bytevector p) => (bytevector))
) ;let

(check-report)
