;; vector%for-each
;; 遍历向量执行副作用（对标 Scala Vector.foreach）。
;;
;; 语法
;; ----
;; ($ v :for-each f)
;;
;; 参数
;; ----
;; f : procedure?
;; 单参过程，施加副作用后返回原向量。
;;
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; vector?
;; 返回原向量 v 本身（保持可链），副作用经由闭包观察。
;;
;; 描述
;; ----
;; vector 路径去糖为 (vector-for-each f v) 后透传 v。

(import (liii check) (liii lang) (scheme base))

(check-set-mode! 'report-failed)

(let ((acc '()))
  (check
    ($ #(1 2 3) :for-each (lambda (x) (set! acc (cons x acc))))
    =>
    #(1 2 3)
  ) ;check
  (check acc => '(3 2 1))
) ;let

(check-report)
