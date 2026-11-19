;; advance-cursor-line!
;; 将光标下移一行；到达屏幕底部时整体上滚一行。
;;
;; 语法
;; ----
;; (advance-cursor-line! state)
;;
;; 参数
;; ----
;; state : vt-state?
;; 虚拟终端状态实例。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui terminal virtual state))

(check-set-mode! 'report-failed)

;; 未到底部：列复位，行号加一
(let ((state (make-vt-state 3 3 2 0 #t (make-screen-lines 3 3) '() '())))
  (advance-cursor-line! state)
  (check (vt-cursor-x state) => 0)
  (check (vt-cursor-y state) => 1)
) ;let

;; 到达底部：整体上滚一行，光标停在最后一行
(let* ((state (make-vt-state 3 2 0 1 #t (make-screen-lines 3 2) '() '()))
       (row0 (make-char-row 3))
       (row1 (make-char-row 3))
      ) ;
  (vector-set! row0 0 #\a)
  (vector-set! row1 0 #\b)
  (vector-set! (vt-lines state) 0 row0)
  (vector-set! (vt-lines state) 1 row1)
  (advance-cursor-line! state)
  (check (vt-cursor-y state) => 1)
  (check (vector->list (vector-ref (vt-lines state) 0))
    =>
    (list #\b #\space #\space)
  ) ;check
) ;let*

(check-report)
