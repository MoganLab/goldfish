(import (liii check)
        (liii base)
        (liii error)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-append!
;; 原地向数组末尾追加元素，并返回同一输入句柄。
;;
;; 语法
;; ----
;; (njson-append! json value)
;; (njson-append! json k1 k2 ... kn value)
;;
;; 参数
;; ----
;; json : njson-handle
;; k1..kn : string | integer
;; value : njson-handle | string | number | boolean | 'null
;;
;; 返回值
;; ----
;; njson-handle
;; 返回更新后的原句柄。
;;
;; 注意
;; ----
;; 目标必须是 array；路径不存在或缺少 value 都会报错。
;;
;; 错误处理
;; ----
;; type-error
;; json 非句柄、句柄已释放或 value 非法时抛出。
;; key-error
;; 缺少 value、路径非法、路径不存在或目标不是数组时抛出。

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

(let-njson ((root (string->njson sample-json)))
  (check-true (njson? (njson-append! root "nums" 99)))
  (check (njson-ref root "nums" 5) => 99)
) ;let-njson

(let-njson ((arr (string->njson "[1,2]")))
  (njson-append! arr 3)
  (check (njson-ref arr 2) => 3)
) ;let-njson

(check-catch 'type-error (njson-append! 'foo 1))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'type-error (njson-append! root "nums" +nan.0))
  (check-catch 'type-error (njson-append! root "nums" +inf.0))
  (check-catch 'type-error (njson-append! root "nums" -inf.0))
  (check (capture-type-error-message (lambda () (njson-append! root "nums" +nan.0)))
         => "g_njson-append!: number must be finite (NaN/Inf are not valid JSON numbers)"
  ) ;check
  (check-catch 'key-error (njson-append! root))
  (check-catch 'key-error (njson-append! root "as"))
  (check (capture-key-error-message (lambda () (njson-append! root "as")))
         => "g_njson-append!: append target must be array"
  ) ;check
  (check-catch 'key-error (njson-append! root "nums"))
  (check (capture-key-error-message (lambda () (njson-append! root "nums")))
         => "g_njson-append!: append target must be array"
  ) ;check
  (check-catch 'key-error (njson-append! root "name" 1))
  (check (capture-key-error-message (lambda () (njson-append! root "name" 1)))
         => "g_njson-append!: append target must be array"
  ) ;check
) ;let-njson

(check-report)
