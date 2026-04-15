(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; flexvector-copy!
;; 将源向量的内容复制到目标向量的指定位置。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-copy! to at from)
;; (flexvector-copy! to at from start)
;; (flexvector-copy! to at from start end)
;;
;; 参数
;; ----
;; to : flexvector
;;   目标向量，会被修改。
;;
;; at : exact-nonnegative-integer
;;   在目标向量中的起始位置。
;;
;; from : flexvector
;;   源向量。
;;
;; start : exact-nonnegative-integer (可选，默认 0)
;;   源向量的起始索引。
;;
;; end : exact-nonnegative-integer (可选，默认长度)
;;   源向量的结束索引（不包含）。
;;
;; 返回值
;; -----
;; 返回值未指定。
;;
;; 副作用
;; -----
;; 修改 to，从 at 位置开始覆盖或插入 from 的元素。
;;
;; 另见
;; ----
;; flexvector-copy - 创建副本
;; flexvector-append! - 追加向量


;; 基本复制
(let ((to (flexvector 1 2 3 4 5))
      (from (flexvector 20 30 40))
     ) ;
  (flexvector-copy! to 1 from)
  (check (flexvector->list to)
    =>
    '(1 20 30 40 5)
  ) ;check
) ;let


;; 复制指定区间
(let ((to (flexvector 1 2 3 4 5))
      (from (flexvector 10 20 30 40 50))
     ) ;
  (flexvector-copy! to 1 from 1 4)
  (check (flexvector->list to)
    =>
    '(1 20 30 40 5)
  ) ;check
) ;let


;; 复制到开头
(let ((to (flexvector 'a 'b 'c))
      (from (flexvector 1 2))
     ) ;
  (flexvector-copy! to 0 from)
  (check (flexvector->list to)
    =>
    '(1 2 c)
  ) ;check
) ;let


;; 复制到末尾
(let ((to (flexvector 'a 'b))
      (from (flexvector 'x 'y 'z))
     ) ;
  (flexvector-copy! to 2 from)
  (check (flexvector->list to)
    =>
    '(a b x y z)
  ) ;check
) ;let


;; 源向量比目标长（目标会扩容）
(let ((to (flexvector 1))
      (from (flexvector 'a 'b 'c 'd 'e))
     ) ;
  (flexvector-copy! to 0 from)
  (check (flexvector->list to)
    =>
    '(a b c d e)
  ) ;check
) ;let


;; 空源向量（无变化）
(let ((to (flexvector 1 2 3))
      (from (flexvector))
     ) ;
  (flexvector-copy! to 1 from)
  (check (flexvector->list to)
    =>
    '(1 2 3)
  ) ;check
) ;let


(check-report)
