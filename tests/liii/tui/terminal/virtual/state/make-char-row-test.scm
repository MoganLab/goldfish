;; make-char-row
;; 创建一行由指定宽度空格组成的字符向量。
;;
;; 语法
;; ----
;; (make-char-row width)
;;
;; 参数
;; ----
;; width : integer?
;; 行宽度（字符个数）。
;;
;; 返回值
;; ----
;; vector?
;; 长度为 width 的字符向量，初始全部为空格。

(import (scheme base) (liii check) (liii tui terminal virtual state))

(check-set-mode! 'report-failed)

(let ((row (make-char-row 5)))
  (check (vector? row) => #t)
  (check (vector-length row) => 5)
  (check (vector->list row) => (list #\space #\space #\space #\space #\space))
) ;let

(let ((row (make-char-row 0)))
  (check (vector-length row) => 0)
) ;let

(check-report)
