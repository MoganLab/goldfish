;; scroll-up-one-line!
;; 将屏幕内容整体上移一行，底部补入一行空白。
;;
;; 语法
;; ----
;; (scroll-up-one-line! state)
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

(let* ((state (make-vt-state 3 2 0 0 #t (make-screen-lines 3 2) '() '()))
       (row0 (make-char-row 3))
       (row1 (make-char-row 3))
      ) ;
  (vector-set! row0 0 #\a)
  (vector-set! row1 0 #\b)
  (vector-set! (vt-lines state) 0 row0)
  (vector-set! (vt-lines state) 1 row1)
  (scroll-up-one-line! state)
  ;; 第一行变成原来的第二行，末行为新空白行
  (check (vector->list (vector-ref (vt-lines state) 0))
    =>
    (list #\b #\space #\space)
  ) ;check
  (check (vector->list (vector-ref (vt-lines state) 1))
    =>
    (list #\space #\space #\space)
  ) ;check
) ;let*

(check-report)
