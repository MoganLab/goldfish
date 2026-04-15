(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; flexvector-append!
;; 将其他 flexvector 的元素追加到第一个 flexvector 末尾。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-append! fv fv2 ...)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量，会被修改。
;;
;; fv2 ... : flexvector
;;   要追加的源向量。
;;
;; 返回值
;; -----
;; 返回修改后的 flexvector（即输入的 fv）。
;;
;; 副作用
;; -----
;; 修改 fv，在其末尾添加其他向量的元素。
;;
;; 另见
;; ----
;; flexvector-append - 非破坏性拼接
;; flexvector-concatenate - 连接向量列表
;; flexvector-copy! - 复制到指定位置


;; 基本测试：追加一个向量
(let ((fv (flexvector 10 20)))
  (flexvector-append! fv
    (flexvector 30 40)
  ) ;flexvector-append!
  (check (flexvector->vector fv)
    =>
    #(10 20 30 40)
  ) ;check
) ;let


;; 追加多个向量
(let ((fv (flexvector 1)))
  (flexvector-append! fv
    (flexvector 2 3)
    (flexvector 4 5)
  ) ;flexvector-append!
  (check (flexvector->list fv)
    =>
    '(1 2 3 4 5)
  ) ;check
) ;let


;; 追加空向量（无变化）
(let ((fv (flexvector 'a 'b)))
  (flexvector-append! fv (flexvector))
  (check (flexvector->list fv) => '(a b))
) ;let


;; 从空向量追加
(let ((fv (flexvector)))
  (flexvector-append! fv
    (flexvector 1 2)
    (flexvector 3)
  ) ;flexvector-append!
  (check (flexvector->list fv)
    =>
    '(1 2 3)
  ) ;check
) ;let


;; 返回值是原对象
(let ((fv (flexvector 1 2)))
  (check (eq? (flexvector-append! fv (flexvector 3))
           fv
         ) ;eq?
    =>
    #t
  ) ;check
) ;let


;; 追加自己
(let ((fv (flexvector 'a 'b)))
  (flexvector-append! fv fv)
  (check (flexvector->list fv)
    =>
    '(a b a b)
  ) ;check
) ;let


;; 大量追加（测试扩容）
(let ((fv (flexvector 1))
      (fv2 (flexvector 2 3 4 5 6 7 8 9 10))
     ) ;
  (flexvector-append! fv fv2)
  (check (flexvector-length fv) => 10)
) ;let


(check-report)
