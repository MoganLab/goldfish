(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; write-string
;; 向输出端口写入字符串。
;;
;; 语法
;; ----
;; (write-string str [port [start [end]]])

;; 写入整个字符串
(let ((p (open-output-string)))
  (write-string "hello" p)
  (check (get-output-string p) => "hello")
) ;let

;; 带 start/end 范围
(let ((p (open-output-string)))
  (write-string "abcdef" p 2 4)
  (check (get-output-string p) => "cd")
) ;let

(check-report)
