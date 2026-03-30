(import (liii check)
        (liii base)
        (liii error)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-contains-key?
;; 判断对象是否包含指定键。
;;
;; 语法
;; ----
;; (njson-contains-key? json key)
;;
;; 参数
;; ----
;; json : njson-handle
;; key : string
;;
;; 返回值
;; ----
;; boolean?
;; 对象包含 key 返回 #t；非对象或不存在返回 #f。
;;
;; 注意
;; ----
;; 非对象值不会抛错，而是直接返回 #f。
;;
;; 错误处理
;; ----
;; type-error
;; json 非句柄或句柄已释放时抛出。
;; key-error
;; key 不是字符串时抛出。

(define sample-json
  "{\"name\":\"Goldfish\",\"version\":\"17.11.26\",\"active\":true,\"score\":3.14,\"nums\":[1,2,3,4,5],\"meta\":{\"arch\":\"x86_64\",\"os\":\"linux\"}}"
) ;define

(define (capture-key-error-message thunk)
  (catch 'key-error
    thunk
    (lambda args
      (let ((payload (if (and (pair? args) (pair? (cdr args))) (cadr args) '())))
        (if (and (pair? payload) (string? (car payload)))
            (car payload)
            ""
        ) ;if
      ) ;let
    ) ;lambda
  ) ;catch
) ;define

(let-njson ((root (string->njson sample-json)))
  (check-true (njson-contains-key? root "meta"))
  (check-false (njson-contains-key? root "not-found"))
) ;let-njson

(let-njson ((arr (string->njson "[1,2]"))
            (scalar (string->njson "1")))
  (check-false (njson-contains-key? arr "0"))
  (check-false (njson-contains-key? scalar "x"))
) ;let-njson

(check-catch 'type-error (njson-contains-key? 'foo "meta"))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-contains-key? root 1))
  (check (capture-key-error-message (lambda () (njson-contains-key? root 1)))
         => "g_njson-contains-key?: json object key must be string?"
  ) ;check
) ;let-njson

(define njson-contains-freed (string->njson "{\"k\":1}"))
(check-true (njson-free njson-contains-freed))
(check-catch 'type-error (njson-contains-key? njson-contains-freed "k"))

(check-report)
