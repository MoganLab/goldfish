(import (liii check)
  (liii base)
  (liii error)
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; njson-append
;; 函数式向数组末尾追加元素，返回新句柄。
;;
;; 语法
;; ----
;; (njson-append json value)
;; (njson-append json k1 k2 ... kn value)
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
;; 追加后的新句柄。
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


(let-njson ((root (string->njson sample-json))
            (root2 (njson-append root "nums" 99))
           ) ;
  (check (njson-ref root2 "nums" 5) => 99)
  (check (njson-size (njson-ref root "nums"))
    =>
    5
  ) ;check
) ;let-njson


(let-njson ((arr (string->njson "[1,2]"))
            (arr2 (njson-append arr 3))
           ) ;
  (check (njson-ref arr2 2) => 3)
  (check (njson-size arr) => 2)
) ;let-njson


(check-catch 'type-error
  (njson-append 'foo 1)
) ;check-catch
(let-njson ((root (string->njson sample-json)))
  (check-catch 'type-error
    (njson-append root "nums" +nan.0)
  ) ;check-catch
  (check-catch 'type-error
    (njson-append root "nums" +inf.0)
  ) ;check-catch
  (check-catch 'type-error
    (njson-append root "nums" -inf.0)
  ) ;check-catch
  (check (capture-type-error-message (lambda ()
                                       (njson-append root "nums" +nan.0)
                                     ) ;lambda
         ) ;capture-type-error-message
    =>
    "g_njson-append: number must be finite (NaN/Inf are not valid JSON numbers)"
  ) ;check
  (check-catch 'key-error
    (njson-append root)
  ) ;check-catch
  (check-catch 'key-error
    (njson-append root "as")
  ) ;check-catch
  (check (capture-key-error-message (lambda () (njson-append root "as"))
         ) ;capture-key-error-message
    =>
    "g_njson-append: append target must be array"
  ) ;check
  (check-catch 'key-error
    (njson-append root "nums")
  ) ;check-catch
  (check (capture-key-error-message (lambda () (njson-append root "nums"))
         ) ;capture-key-error-message
    =>
    "g_njson-append: append target must be array"
  ) ;check
  (check-catch 'key-error
    (njson-append root "name" 1)
  ) ;check-catch
  (check (capture-key-error-message (lambda () (njson-append root "name" 1))
         ) ;capture-key-error-message
    =>
    "g_njson-append: append target must be array"
  ) ;check
) ;let-njson


(check-report)
