(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; bytevector-copy!
;; 将 from 中的字节复制到 to 的指定位置。
;;
;; 语法
;; ----
;; (bytevector-copy! to at from [start [end]])
;;
;; 返回值
;; ----
;; to

;; 基本复制
(let ((to (make-bytevector 4)))
  (check (bytevector-copy! to 0 (bytevector 1 2 3)) => to)
  (check to => (bytevector 1 2 3 0))
) ;let

;; 复制到非零位置
(let ((to (make-bytevector 4)))
  (bytevector-copy! to 1 (bytevector 9 8 7))
  (check to => (bytevector 0 9 8 7))
) ;let

;; 带 start/end 范围
(let ((to (make-bytevector 4)))
  (bytevector-copy! to 0 (bytevector 1 2 3 4) 1 3)
  (check to => (bytevector 2 3 0 0))
) ;let

;; 返回 to
(check (eq? (bytevector-copy! (make-bytevector 3) 0 (bytevector 1 2 3))
         (make-bytevector 3)
       ) ;eq?
  =>
  #f
) ;check

(check-report)
