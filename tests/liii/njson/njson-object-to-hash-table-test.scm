(import (liii check)
        (liii base)
        (liii hash-table)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-object->hash-table
;; 把 njson object 递归转换为 hash-table/vector 家族的纯 Scheme 结构。
;;
;; 语法
;; ----
;; (njson-object->hash-table object-json)
;;
;; 参数
;; ----
;; object-json : njson-handle
;; 必须是指向 JSON object 的句柄。
;;
;; 返回值
;; ----
;; hash-table
;; 返回纯 Scheme 的 hash-table/vector 结构。
;;
;; 注意
;; ----
;; 转换结果不依赖 njson 句柄生命周期。
;;
;; 错误处理
;; ----
;; type-error
;; 输入不是 object-handle 或句柄已释放时抛出。

(define njson-object->hash-table-json
  "{\"name\":\"Goldfish\",\"meta\":{\"os\":\"linux\",\"empty\":{}},\"nums\":[1,{\"deep\":true},[]],\"nil\":null}"
) ;define

(define object-as-hash-table #f)
(let-njson ((root (string->njson njson-object->hash-table-json)))
  (set! object-as-hash-table (njson-object->hash-table root))
  (check-true (hash-table? object-as-hash-table))
  (check (hash-table-ref object-as-hash-table "name") => "Goldfish")
  (let ((meta (hash-table-ref object-as-hash-table "meta"))
        (nums (hash-table-ref object-as-hash-table "nums")))
    (check-true (hash-table? meta))
    (check (hash-table-ref meta "os") => "linux")
    (check-true (hash-table? (hash-table-ref meta "empty")))
    (check (hash-table-size (hash-table-ref meta "empty")) => 0)
    (check-true (vector? nums))
    (check (vector-ref nums 0) => 1)
    (check-true (hash-table? (vector-ref nums 1)))
    (check (hash-table-ref (vector-ref nums 1) "deep") => #t)
    (check (vector-ref nums 2) => #())
  ) ;let
  (check (hash-table-ref object-as-hash-table "nil") => 'null)
) ;let-njson
(check (hash-table-ref object-as-hash-table "name") => "Goldfish")

(let-njson ((root (string->njson "{}")))
  (let ((ht (njson-object->hash-table root)))
    (check-true (hash-table? ht))
    (check (hash-table-size ht) => 0)
  ) ;let
) ;let-njson

(check-catch 'type-error (njson-object->hash-table 'foo))
(let-njson ((scalar (string->njson "1")))
  (check-catch 'type-error (njson-object->hash-table scalar))
) ;let-njson
(define object->hash-table-freed (string->njson "{\"a\":1}"))
(check-true (njson-free object->hash-table-freed))
(check-catch 'type-error (njson-object->hash-table object->hash-table-freed))

(check-report)
