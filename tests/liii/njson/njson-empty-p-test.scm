(import (liii check)
  (liii base)
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; njson-empty?
;; 判断 JSON 容器是否为空。
;;
;; 语法
;; ----
;; (njson-empty? json)
;;
;; 参数
;; ----
;; json : njson-handle
;;
;; 返回值
;; ----
;; boolean?
;; 容器为空或非容器时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; scalar 和 null 按“空”处理。
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
  (check-false (njson-empty? root))
  (check-false (njson-empty? arr))
  (check-true (njson-empty? empty-obj))
  (check-true (njson-empty? empty-arr))
  (check-true (njson-empty? scalar))
) ;let-njson


(check-catch 'type-error
  (njson-empty? 'foo)
) ;check-catch


(define njson-empty-freed
  (string->njson "{\"k\":1}")
) ;define
(check-true (njson-free njson-empty-freed)
) ;check-true
(check-catch 'type-error
  (njson-empty? njson-empty-freed)
) ;check-catch


(check-report)
