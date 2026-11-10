;; read-bytevector!
;; 从二进制输入端口读取字节填充到已有字节向量中。
;;
;; 语法
;; ----
;; (read-bytevector! bytevector)
;; (read-bytevector! bytevector port)
;; (read-bytevector! bytevector port start)
;; (read-bytevector! bytevector port start end)
;;
;; 参数
;; ----
;; bytevector : bytevector?
;; 目标字节向量。
;;
;; port : input-port? (可选)
;; 二进制输入端口，默认为当前输入端口。
;;
;; start : integer? (可选)
;; 填充起始索引（包含），默认为 0。
;;
;; end : integer? (可选)
;; 填充结束索引（不包含），默认为字节向量长度。
;;
;; 返回值
;; ----
;; integer? 或 eof-object?
;; 实际读取的字节数；若无可读取字节且到达文件末尾则返回 EOF 对象。
;; 若 end = start 则返回 0。
;;
;; 错误处理
;; --------
;; type-error 当参数类型不匹配时抛出。
;; out-of-range 当 start 或 end 超出范围时抛出。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 完整填充
(let* ((p (open-input-bytevector (bytevector 1 2 3)))
       (bv (make-bytevector 3 0)))
  (check (read-bytevector! bv p) => 3)
  (check bv => (bytevector 1 2 3))
  (close-port p)
) ;let

;; 部分读取（端口数据不足）
(let* ((p (open-input-bytevector (bytevector 10 20)))
       (bv (make-bytevector 5 0)))
  (check (read-bytevector! bv p) => 2)
  (check bv => (bytevector 10 20 0 0 0))
  ;; 下一次读取立即遇到 EOF
  (check (eof-object? (read-bytevector! bv p)) => #t)
  (close-port p)
) ;let

;; 指定 start 和 end
(let* ((p (open-input-bytevector (bytevector 7 8)))
       (bv (make-bytevector 4 0)))
  (check (read-bytevector! bv p 1 3) => 2)
  (check bv => (bytevector 0 7 8 0))
  (close-port p)
) ;let

;; start = end：返回 0，不消耗字节
(let* ((p (open-input-bytevector (bytevector 99)))
       (bv (make-bytevector 2 0)))
  (check (read-bytevector! bv p 1 1) => 0)
  (check (read-u8 p) => 99)
  (close-port p)
) ;let

;; 默认从 current-input-port 读取
(let ((orig (current-input-port))
      (p (open-input-bytevector (bytevector 4 5 6)))
      (bv (make-bytevector 3 0)))
  (set-current-input-port p)
  (check (read-bytevector! bv) => 3)
  (check bv => (bytevector 4 5 6))
  (set-current-input-port orig)
  (close-port p)
) ;let

;; 错误处理：类型检查
(check-catch 'type-error (read-bytevector! "not-bv"))
(let ((p (open-input-bytevector (bytevector 1))))
  (check-catch 'type-error (read-bytevector! (bytevector 0) p "0"))
  (check-catch 'type-error (read-bytevector! (bytevector 0) p 0 "1"))
  (close-port p)
) ;let

;; 错误处理：越界
(let ((p (open-input-bytevector (bytevector 1))))
  (check-catch 'out-of-range (read-bytevector! (bytevector 0) p -1))
  (check-catch 'out-of-range (read-bytevector! (bytevector 0) p 0 2))
  (check-catch 'out-of-range (read-bytevector! (bytevector 0) p 1 0))
  (close-port p)
) ;let

;; 错误处理：已关闭的端口
(let ((p (open-input-bytevector (bytevector 1))))
  (close-input-port p)
  (check-catch 'type-error (read-bytevector! (bytevector 0) p))
) ;let

(check-report)
