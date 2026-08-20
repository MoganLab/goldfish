(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; read-bytevector
;; 从输入端口读取至多 k 个字节。
;;
;; 语法
;; ----
;; (read-bytevector k [port])

;; 读取全部字节
(check (read-bytevector 3 (open-input-bytevector (bytevector 65 66 67)))
       => (bytevector 65 66 67))

;; 读取少于 k 个字节（数据不足时返回可用部分）
(check (read-bytevector 5 (open-input-bytevector (bytevector 65 66)))
       => (bytevector 65 66))

;; 空输入返回 eof-object
(check (eof-object? (read-bytevector 3 (open-input-bytevector (bytevector))))
       => #t)

(check-report)
