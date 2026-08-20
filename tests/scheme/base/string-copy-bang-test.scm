(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; string-copy!
;; 将 from 中的字符复制到 to 的指定位置。
;;
;; 语法
;; ----
;; (string-copy! to at from [start [end]])
;;
;; 返回值
;; ----
;; to

;; 基本复制
(let ((to (make-string 4 #\x)))
  (check (string-copy! to 0 "abc") => to)
  (check to => "abcx")
) ;let

;; 复制到非零位置
(let ((to (make-string 5 #\x)))
  (string-copy! to 1 "ab")
  (check to => "xabxx")
) ;let

;; 带 start/end 范围
(let ((to (make-string 4 #\.)))
  (string-copy! to 0 "abcdef" 2 4)
  (check to => "cd..")
) ;let

(check-report)
