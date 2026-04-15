(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; flexvector-remove-range!
;; 移除 flexvector 中指定范围的元素。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-remove-range! fv start end)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量，会被修改。
;;
;; start : exact-nonnegative-integer
;;   起始索引（包含）。
;;
;; end : exact-nonnegative-integer
;;   结束索引（不包含）。
;;
;; 返回值
;; -----
;; 返回修改后的 flexvector（即输入的 fv）。
;;
;; 副作用
;; -----
;; 修改 fv，移除 [start, end) 范围内的元素，后续元素前移。
;;
;; 另见
;; ----
;; flexvector-remove! - 移除单个元素
;; flexvector-clear! - 清空向量


;; 基本测试：移除中间范围
(let ((fv (flexvector 'a 'b 'c 'd 'e 'f)))
  (flexvector-remove-range! fv 1 4)
  (check (flexvector->list fv)
    =>
    '(a e f)
  ) ;check
) ;let


;; 空范围（无变化）
(let ((fv (flexvector 'a 'b 'c 'd 'e 'f)))
  (flexvector-remove-range! fv 1 1)
  (check (flexvector->list fv)
    =>
    '(a b c d e f)
  ) ;check
) ;let


;; 移除前缀
(let ((fv (flexvector 'a 'b 'c 'd)))
  (flexvector-remove-range! fv 0 2)
  (check (flexvector->list fv) => '(c d))
) ;let


;; 移除后缀
(let ((fv (flexvector 'a 'b 'c 'd)))
  (flexvector-remove-range! fv 2 4)
  (check (flexvector->list fv) => '(a b))
) ;let


;; 移除整个向量
(let ((fv (flexvector 'a 'b 'c)))
  (flexvector-remove-range! fv 0 3)
  (check (flexvector-empty? fv) => #t)
) ;let


;; 边界超出
(let ((fv (flexvector 'a 'b 'c)))
  (flexvector-remove-range! fv -1 2)
  (check (flexvector->list fv) => '(c))
) ;let


;; 返回值是原对象
(let ((fv (flexvector 'a 'b 'c)))
  (check (eq? (flexvector-remove-range! fv 0 1)
           fv
         ) ;eq?
    =>
    #t
  ) ;check
) ;let


(check-report)
