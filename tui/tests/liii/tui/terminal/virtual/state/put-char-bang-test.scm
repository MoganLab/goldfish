;; put-char!
;; 在光标位置写入一个字符，并把光标向右推进一格。
;;
;; 语法
;; ----
;; (put-char! state ch)
;;
;; 参数
;; ----
;; state : vt-state?
;; 虚拟终端状态实例。
;;
;; ch : char?
;; 待写入的字符。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui terminal virtual state))

(check-set-mode! 'report-failed)

;; 正常写入
(let ((state (make-vt-state 5 2 0 0 #t (make-screen-lines 5 2) '() '())))
  (put-char! state #\a)
  (check (vt-cursor-x state) => 1)
  (check (vector-ref (vector-ref (vt-lines state) 0) 0) => #\a)
  (put-char! state #\b)
  (check (vt-cursor-x state) => 2)
  (check (vector-ref (vector-ref (vt-lines state) 0) 1) => #\b)
) ;let

;; 超出右边界时不写入，光标仍然前进
(let ((state (make-vt-state 2 1 2 0 #t (make-screen-lines 2 1) '() '())))
  (put-char! state #\x)
  (check (vt-cursor-x state) => 3)
  (check (vector-ref (vector-ref (vt-lines state) 0) 0) => #\space)
) ;let

(check-report)
