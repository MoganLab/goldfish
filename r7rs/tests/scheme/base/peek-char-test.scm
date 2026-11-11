;; peek-char
;; 查看输入端口的下一个字符，但不消费。
;;
;;
;; 语法
;; ----
;; (peek-char)
;; (peek-char port)
;;
;; 参数
;; ----
;; port : input-port?
;; 可选，默认为当前输入端口。
;;
;; 返回值
;; ----
;; char? 或 eof-object?
;; 下一个字符，或 EOF 对象。
;;
;; 说明
;; ----
;; 1. 查看后端口位置不变
;; 2. 可多次连续 peek 得到相同结果

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(let ((p (open-input-string "abc")))
  (check (peek-char p) => #\a)
  (check (peek-char p) => #\a)
  (check (read-char p) => #\a)
  (check (peek-char p) => #\b)
) ;let

;; 错误处理：二进制端口不是文本端口
(let ((p (open-input-bytevector (bytevector 65 66))))
  (check-catch 'type-error (peek-char p))
) ;let

;; 错误处理：已关闭的端口
(let ((p (open-input-string "abc")))
  (close-input-port p)
  (check-catch 'type-error (peek-char p))
) ;let

(check-report)
