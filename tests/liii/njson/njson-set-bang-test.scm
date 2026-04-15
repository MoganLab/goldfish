(import (liii check)
  (liii base)
  (liii error)
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; njson-set!
;; 原地更新 JSON，直接修改输入句柄。
;;
;; 语法
;; ----
;; (njson-set! json key ... value)
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
;; 返回已更新的原句柄。
;;
;; 注意
;; ----
;; 更新成功后会影响原句柄上的后续读取。
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
      (let ((payload (if (and (pair? args) (pair? (cdr args)))
                       (cadr args)
                       '()
                     ) ;if
            ) ;payload
           ) ;
        (if (and (pair? payload)
              (string? (car payload))
            ) ;and
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
      (let ((payload (if (and (pair? args) (pair? (cdr args)))
                       (cadr args)
                       '()
                     ) ;if
            ) ;payload
           ) ;
        (if (and (pair? payload)
              (string? (car payload))
            ) ;and
          (car payload)
          ""
        ) ;if
      ) ;let
    ) ;lambda
  ) ;catch
) ;define


(let-njson ((root (string->njson sample-json)))
  (check-true (njson? (njson-set! root "meta" "os" "debian")
              ) ;njson?
  ) ;check-true
  (njson-set! root "city" "HZ")
  (njson-set! root "nums" 4 99)
  (check (njson-ref root "meta" "os")
    =>
    "debian"
  ) ;check
  (check (njson-ref root "city") => "HZ")
  (check (njson-ref root "nums" 4) => 99)
) ;let-njson


(let-njson ((root (string->njson sample-json))
            (meta (njson-ref root "meta"))
           ) ;
  (njson-set! root "meta-copy" meta)
  (check (njson-ref root "meta-copy" "arch")
    =>
    "x86_64"
  ) ;check
  (check-false (njson-contains-key? meta "missing")
  ) ;check-false
) ;let-njson


(check-catch 'type-error
  (njson-set! 'foo "meta" "os" "debian")
) ;check-catch
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error
    (njson-set! root 'meta "os" "debian")
  ) ;check-catch
) ;let-njson
(let-njson ((root (string->njson sample-json)))
  (check-catch 'type-error
    (njson-set! root "score" +nan.0)
  ) ;check-catch
  (check-catch 'type-error
    (njson-set! root "score" +inf.0)
  ) ;check-catch
  (check-catch 'type-error
    (njson-set! root "score" -inf.0)
  ) ;check-catch
  (check (capture-type-error-message (lambda ()
                                       (njson-set! root "score" +nan.0)
                                     ) ;lambda
         ) ;capture-type-error-message
    =>
    "g_njson-set!: number must be finite (NaN/Inf are not valid JSON numbers)"
  ) ;check
) ;let-njson
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error
    (njson-set! root "nums" 5 1)
  ) ;check-catch
  (check (capture-key-error-message (lambda () (njson-set! root "nums" 5 1))
         ) ;capture-key-error-message
    =>
    "g_njson-set!: array index out of range (index=5, size=5)"
  ) ;check
) ;let-njson
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error
    (njson-set! root "meta" "missing" "k" 1)
  ) ;check-catch
  (check (capture-key-error-message (lambda ()
                                      (njson-set! root "meta" "missing" "k" 1)
                                    ) ;lambda
         ) ;capture-key-error-message
    =>
    "g_njson-set!: path not found: missing object key 'missing'"
  ) ;check
) ;let-njson
(let-njson ((root (string->njson "1")))
  (check-catch 'key-error
    (njson-set! root "x" 1)
  ) ;check-catch
  (check (capture-key-error-message (lambda () (njson-set! root "x" 1))
         ) ;capture-key-error-message
    =>
    "g_njson-set!: set target must be array or object"
  ) ;check
) ;let-njson
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error
    (njson-set! root "name" "x" "y")
  ) ;check-catch
  (check (capture-key-error-message (lambda ()
                                      (njson-set! root "name" "x" "y")
                                    ) ;lambda
         ) ;capture-key-error-message
    =>
    "g_njson-set!: set target must be array or object"
  ) ;check
) ;let-njson


(check-report)
