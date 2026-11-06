;; make-screen-lines
;; 创建由若干空行组成的屏幕行向量。
;;
;; 语法
;; ----
;; (make-screen-lines width height)
;;
;; 参数
;; ----
;; width : integer?
;; 每行的宽度（字符个数）。
;;
;; height : integer?
;; 屏幕行数。
;;
;; 返回值
;; ----
;; vector?
;; 长度为 height 的字符行向量。

(import (scheme base) (liii check) (liii tui terminal virtual state))

(check-set-mode! 'report-failed)

(let ((lines (make-screen-lines 4 3)))
  (check (vector? lines) => #t)
  (check (vector-length lines) => 3)
  (check (vector-length (vector-ref lines 0)) => 4)
  (check (vector->list (vector-ref lines 0))
    =>
    (list #\space #\space #\space #\space)
  ) ;check
  (check (vector->list (vector-ref lines 2))
    =>
    (list #\space #\space #\space #\space)
  ) ;check
) ;let

(let ((lines (make-screen-lines 2 0)))
  (check (vector-length lines) => 0)
) ;let

(check-report)
