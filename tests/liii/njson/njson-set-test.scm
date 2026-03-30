(import (liii check)
        (liii base)
        (liii error)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-set
;; 函数式更新 JSON，返回新句柄而不修改原句柄。
;;
;; 语法
;; ----
;; (njson-set json key ... value)
;;
;; 参数
;; ----
;; json : njson-handle
;; key ... : string | integer
;; value : njson-handle | string | number | boolean | 'null
;;
;; 返回值
;; ----
;; njson-handle
;; 更新后的新句柄。
;;
;; 注意
;; ----
;; 中间路径必须存在，array 越界会抛 key-error。
;;
;; 错误处理
;; ----
;; type-error
;; json 非句柄、句柄已释放或 value 非法时抛出。
;; key-error
;; 路径非法、路径不存在或数组索引越界时抛出。

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

(define (capture-type-error-message thunk)
  (catch 'type-error
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

(let-njson ((root (string->njson sample-json))
            (root2 (njson-set root "meta" "os" "debian"))
            (root3 (njson-set root "city" "HZ"))
            (root4 (njson-set root "nums" 4 99)))
  (check (njson-ref root2 "meta" "os") => "debian")
  (check (njson-ref root "meta" "os") => "linux")
  (check (njson-ref root3 "city") => "HZ")
  (check-false (njson-contains-key? root "city"))
  (check (njson-ref root4 "nums" 4) => 99)
  (check (njson-size (njson-ref root "nums")) => 5)
) ;let-njson

(let-njson ((root (string->njson sample-json))
            (root-idx-update (njson-set root "nums" 1 200))
            (meta (njson-ref root "meta"))
            (root-handle-value (njson-set root "meta-copy" meta)))
  (check (njson-ref root-idx-update "nums" 1) => 200)
  (check (njson-ref root-handle-value "meta-copy" "os") => "linux")
  (check-false (njson-contains-key? root "meta-copy"))
) ;let-njson

(check-catch 'type-error (njson-set 'foo "meta" "os" "debian"))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-set root 'meta "os" "debian"))
) ;let-njson
(let-njson ((root (string->njson sample-json)))
  (check-catch 'type-error (njson-set root "score" +nan.0))
  (check-catch 'type-error (njson-set root "score" +inf.0))
  (check-catch 'type-error (njson-set root "score" -inf.0))
  (check (capture-type-error-message (lambda () (njson-set root "score" +nan.0)))
         => "g_njson-set: number must be finite (NaN/Inf are not valid JSON numbers)"
  ) ;check
) ;let-njson
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-set root "nums" 5 1))
  (check (capture-key-error-message (lambda () (njson-set root "nums" 5 1)))
         => "g_njson-set: array index out of range (index=5, size=5)"
  ) ;check
) ;let-njson
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-set root "meta" "missing" "k" 1))
  (check (capture-key-error-message (lambda () (njson-set root "meta" "missing" "k" 1)))
         => "g_njson-set: path not found: missing object key 'missing'"
  ) ;check
) ;let-njson
(let-njson ((root (string->njson "1")))
  (check-catch 'key-error (njson-set root "x" 1))
  (check (capture-key-error-message (lambda () (njson-set root "x" 1)))
         => "g_njson-set: set target must be array or object"
  ) ;check
) ;let-njson
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-set root "name" "x" "y"))
  (check (capture-key-error-message (lambda () (njson-set root "name" "x" "y")))
         => "g_njson-set: set target must be array or object"
  ) ;check
) ;let-njson

(check-report)
