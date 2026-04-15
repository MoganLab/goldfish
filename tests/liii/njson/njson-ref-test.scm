(import (liii check)
  (liii base)
  (liii error)
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; njson-ref
;; 按路径读取 JSON 值，命中容器时返回新的 njson 句柄。
;;
;; 语法
;; ----
;; (njson-ref json key)
;; (njson-ref json k1 k2 ... kn)
;;
;; 参数
;; ----
;; json : njson-handle
;; key / k1..kn : string | integer
;; object 层用字符串键，array 层用整数索引。
;;
;; 返回值
;; ----
;; any?
;; 命中标量返回标量，命中容器返回新的 njson 句柄。
;;
;; 注意
;; ----
;; 路径不存在或 token 类型不匹配时抛 key-error。
;;
;; 错误处理
;; ----
;; type-error
;; 输入不是可用句柄时抛出。
;; key-error
;; 路径非法、路径不存在或 token 与层类型不匹配时抛出。


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


(let-njson ((root (string->njson sample-json)))
  (check (njson-ref root "name")
    =>
    "Goldfish"
  ) ;check
  (check (njson-ref root "active") => #t)
  (check (njson-ref root "meta" "arch")
    =>
    "x86_64"
  ) ;check
  (check-true (njson? (njson-ref root "meta"))
  ) ;check-true
) ;let-njson


(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error
    (njson-ref root 'meta)
  ) ;check-catch
  (check-catch 'key-error
    (njson-ref root "not-found")
  ) ;check-catch
  (check-catch 'key-error
    (njson-ref root "nums" 999)
  ) ;check-catch
  (check-catch 'key-error
    (njson-ref root "name" "x")
  ) ;check-catch
  (check (capture-key-error-message (lambda () (njson-ref root "not-found"))
         ) ;capture-key-error-message
    =>
    "g_njson-ref: path not found: missing object key 'not-found'"
  ) ;check
) ;let-njson


(define functional-meta '())
(let-njson ((root (string->njson sample-json))
            (meta (njson-ref root "meta"))
           ) ;
  (set! functional-meta meta)
  (check (njson-ref meta "os") => "linux")
) ;let-njson
(check-catch 'type-error
  (njson-ref functional-meta "os")
) ;check-catch


(check-catch 'type-error
  (njson-ref 'foo "x")
) ;check-catch


(check-report)
