(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-map!
;; 破坏性映射操作。
;;
;; 语法
;; ----
;; (vector-map! proc vec)
;; (vector-map! proc vec1 vec2 ...)
;;
;; 参数
;; ----
;; proc : procedure?
;; 应用于每组元素的过程。
;;
;; vec, vec1, vec2, ... : vector?
;; 输入向量。第一个向量会被原地修改。
;;
;; 返回值
;; ------
;; vector?
;; 返回被原地修改的第一个向量。
;;
;; 注意
;; ----
;; 多个向量会按对应位置并行映射，以最短向量长度为准。


(let ((v #(1 2 3)))
  (vector-map! (lambda (x) (* x 10)) v)
  (check v => #(10 20 30))
) ;let

(let ((v1 #(1 2 3)) (v2 #(4 5 6)))
  (vector-map! + v1 v2)
  (check v1 => #(5 7 9))
) ;let

(let ((v #(1 2 3)))
  (vector-map! (lambda (x) x) v)
  (check v => #(1 2 3))
) ;let

(let ((v #()))
  (vector-map! (lambda (x) (* x 2)) v)
  (check v => #())
) ;let

(let ((v #(5)))
  (vector-map! (lambda (x) (* x x)) v)
  (check v => #(25))
) ;let

(let ((v1 #(1 2 3)) (v2 #(10 20)))
  (vector-map! + v1 v2)
  (check v1 => #(11 22 3))
) ;let

(check-report)
