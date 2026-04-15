(import (liii check)
  (liii base)
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; njson-size
;; 获取 JSON 容器大小，非容器返回 0。
;;
;; 语法
;; ----
;; (njson-size json)
;;
;; 参数
;; ----
;; json : njson-handle
;;
;; 返回值
;; ----
;; integer?
;; object/array 返回元素个数，其他值返回 0。
;;
;; 注意
;; ----
;; 句柄失效后会抛 type-error。
;;
;; 错误处理
;; ----
;; type-error
;; json 非句柄或句柄已释放时抛出。


(let-njson ((root (string->njson "{\"name\":\"Goldfish\",\"version\":\"17.11.26\",\"active\":true,\"score\":3.14,\"nums\":[1,2,3,4,5],\"meta\":{\"arch\":\"x86_64\",\"os\":\"linux\"}}"
                  ) ;string->njson
            ) ;root
            (arr (string->njson "[1,2,3]"))
            (empty-obj (string->njson "{}"))
            (empty-arr (string->njson "[]"))
            (scalar (string->njson "3.14"))
           ) ;
  (check (njson-size root) => 6)
  (check (njson-size arr) => 3)
  (check (njson-size empty-obj) => 0)
  (check (njson-size empty-arr) => 0)
  (check (njson-size scalar) => 0)
) ;let-njson


(check-catch 'type-error
  (njson-size 'foo)
) ;check-catch


(define njson-size-freed
  (string->njson "{\"k\":1}")
) ;define
(check-true (njson-free njson-size-freed)
) ;check-true
(check-catch 'type-error
  (njson-size njson-size-freed)
) ;check-catch


(check-report)
