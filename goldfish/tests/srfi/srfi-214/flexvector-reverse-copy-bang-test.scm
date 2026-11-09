;; flexvector-reverse-copy!
;; 反向复制到目标向量。
;;
;; 语法
;; ----
;; (flexvector-reverse-copy! to at from)
;; (flexvector-reverse-copy! to at from start)
;; (flexvector-reverse-copy! to at from start end)
;;
;; 参数
;; ----
;; to : flexvector
;; 目标可变长向量。
;;
;; at : exact-integer
;; 目标向量的起始索引。
;;
;; from : flexvector
;; 源可变长向量。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未指定。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(let ((to (flexvector 1 2 3 4 5)) (from (flexvector 20 30 40)))
  (flexvector-reverse-copy! to 1 from)
  (check (flexvector->list to) => '(1 40 30 20 5))
) ;let


(check-report)
