;; copy-row-chars!
;; 把一行字符向量的前 copy-len 个字符复制到目标行向量。
;;
;; 语法
;; ----
;; (copy-row-chars! old-row new-row copy-len)
;;
;; 参数
;; ----
;; old-row : vector?
;; 源字符行向量。
;;
;; new-row : vector?
;; 目标字符行向量。
;;
;; copy-len : integer?
;; 复制的字符个数。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui terminal virtual state))

(check-set-mode! 'report-failed)

(let* ((old-row (make-char-row 4)) (new-row (make-char-row 4)))
  (vector-set! old-row 0 #\a)
  (vector-set! old-row 1 #\b)
  (copy-row-chars! old-row new-row 2)
  (check (vector->list new-row) => (list #\a #\b #\space #\space))
) ;let*

;; 复制 0 个字符不改变目标行
(let* ((old-row (make-char-row 3)) (new-row (make-char-row 3)))
  (vector-set! old-row 0 #\z)
  (copy-row-chars! old-row new-row 0)
  (check (vector->list new-row) => (list #\space #\space #\space))
) ;let*

(check-report)
