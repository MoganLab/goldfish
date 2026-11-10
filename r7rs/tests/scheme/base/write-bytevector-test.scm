;; write-bytevector
;; 将字节向量中的字节写入二进制输出端口。
;;
;; 语法
;; ----
;; (write-bytevector bytevector)
;; (write-bytevector bytevector port)
;; (write-bytevector bytevector port start)
;; (write-bytevector bytevector port start end)
;;
;; 参数
;; ----
;; bytevector : bytevector?
;; 源字节向量。
;;
;; port : output-port? (可选)
;; 二进制输出端口，默认为当前输出端口。
;;
;; start : integer? (可选)
;; 起始索引（包含），默认为 0。
;;
;; end : integer? (可选)
;; 结束索引（不包含），默认为字节向量长度。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 错误处理
;; --------
;; type-error 当参数类型不匹配时抛出。
;; out-of-range 当 start 或 end 超出范围时抛出。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 完整写入
(let ((p (open-output-bytevector)))
  (write-bytevector (bytevector 1 2 3) p)
  (check (get-output-bytevector p) => (bytevector 1 2 3))
  (close-port p)
) ;let

;; 默认写入 current-output-port
(let ((orig (current-output-port))
      (p (open-output-bytevector)))
  (set-current-output-port p)
  (write-bytevector (bytevector 10 20 30))
  (set-current-output-port orig)
  (check (get-output-bytevector p) => (bytevector 10 20 30))
  (close-port p)
) ;let

;; 指定 start
(let ((p (open-output-bytevector)))
  (write-bytevector (bytevector 1 2 3 4 5) p 2)
  (check (get-output-bytevector p) => (bytevector 3 4 5))
  (close-port p)
) ;let

;; 指定 start 和 end
(let ((p (open-output-bytevector)))
  (write-bytevector (bytevector 1 2 3 4 5) p 1 4)
  (check (get-output-bytevector p) => (bytevector 2 3 4))
  (close-port p)
) ;let

;; start = end：写入 0 字节
(let ((p (open-output-bytevector)))
  (write-bytevector (bytevector 1 2 3) p 1 1)
  (check (get-output-bytevector p) => (bytevector))
  (close-port p)
) ;let

;; 错误处理：类型错误
(check-catch 'type-error (write-bytevector "not-bv"))
(let ((p (open-output-bytevector)))
  (check-catch 'type-error (write-bytevector (bytevector 1) p "0"))
  (check-catch 'type-error (write-bytevector (bytevector 1) p 0 "1"))
  (close-port p)
) ;let

;; 错误处理：越界
(let ((p (open-output-bytevector)))
  (check-catch 'out-of-range (write-bytevector (bytevector 1 2) p -1))
  (check-catch 'out-of-range (write-bytevector (bytevector 1 2) p 0 3))
  (check-catch 'out-of-range (write-bytevector (bytevector 1 2) p 2 1))
  (close-port p)
) ;let

;; 错误处理：已关闭端口
(let ((p (open-output-bytevector)))
  (close-output-port p)
  (check-catch 'type-error (write-bytevector (bytevector 1) p))
) ;let

(check-report)
