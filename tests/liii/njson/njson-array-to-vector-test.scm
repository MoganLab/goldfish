(import (liii check)
  (liii base)
  (liii hash-table)
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; njson-array->vector
;; 把 njson array 递归转换为 vector/hash-table 家族的纯 Scheme 结构。
;;
;; 语法
;; ----
;; (njson-array->vector array-json)
;;
;; 参数
;; ----
;; array-json : njson-handle
;; 必须是指向 JSON array 的句柄。
;;
;; 返回值
;; ----
;; vector
;; 返回纯 Scheme 的 vector/hash-table 结构。
;;
;; 注意
;; ----
;; 转换结果不依赖 njson 句柄生命周期。
;;
;; 错误处理
;; ----
;; type-error
;; 输入不是 array-handle 或句柄已释放时抛出。


(define njson-array->vector-json
  "[1,{\"name\":\"Goldfish\",\"tags\":[\"a\",\"b\"]},[2,{\"k\":null}],[]]"
) ;define


(define array-as-vector #())
(let-njson ((root (string->njson njson-array->vector-json)
            ) ;root
           ) ;
  (set! array-as-vector
    (njson-array->vector root)
  ) ;set!
  (check-true (vector? array-as-vector))
  (check (vector-ref array-as-vector 0)
    =>
    1
  ) ;check
  (let ((obj (vector-ref array-as-vector 1))
        (nested (vector-ref array-as-vector 2))
       ) ;
    (check-true (hash-table? obj))
    (check (hash-table-ref obj "name")
      =>
      "Goldfish"
    ) ;check
    (check (hash-table-ref obj "tags")
      =>
      #("a" "b")
    ) ;check
    (check-true (vector? nested))
    (check (vector-ref nested 0) => 2)
    (check-true (hash-table? (vector-ref nested 1))
    ) ;check-true
    (check (hash-table-ref (vector-ref nested 1)
             "k"
           ) ;hash-table-ref
      =>
      'null
    ) ;check
  ) ;let
  (check (vector-ref array-as-vector 3)
    =>
    #()
  ) ;check
) ;let-njson
(check (vector-ref array-as-vector 0)
  =>
  1
) ;check


(let-njson ((root (string->njson "[]")))
  (check (njson-array->vector root)
    =>
    #()
  ) ;check
) ;let-njson


(check-catch 'type-error
  (njson-array->vector 'foo)
) ;check-catch
(let-njson ((scalar (string->njson "1")))
  (check-catch 'type-error
    (njson-array->vector scalar)
  ) ;check-catch
) ;let-njson
(define array->vector-freed
  (string->njson "[1]")
) ;define
(check-true (njson-free array->vector-freed)
) ;check-true
(check-catch 'type-error
  (njson-array->vector array->vector-freed
  ) ;njson-array->vector
) ;check-catch


(check-report)
