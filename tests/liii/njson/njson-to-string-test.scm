(import (liii check)
  (liii base)
  (liii error)
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; njson->string
;; 将 njson 句柄或 strict JSON 标量序列化为紧凑 JSON 字符串。
;;
;; 语法
;; ----
;; (njson->string value)
;;
;; 参数
;; ----
;; value : njson-handle | string | number | boolean | 'null
;;
;; 返回值
;; ----
;; string
;; 紧凑 JSON 文本。
;;
;; 注意
;; ----
;; 结果应可再次被 string->njson 解析。
;;
;; 错误处理
;; ----
;; type-error
;; 输入不是支持的 njson 值时抛出。


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


(check (njson->string 'null) => "null")
(check (njson->string "x") => "\"x\"")
(check (njson->string #f) => "false")


(let-njson ((root (string->njson "{\"b\":1,\"a\":2}")
            ) ;root
           ) ;
  (check (njson->string root)
    =>
    "{\"a\":2,\"b\":1}"
  ) ;check
) ;let-njson


(check-catch 'type-error
  (njson->string +nan.0)
) ;check-catch
(check-catch 'type-error
  (njson->string +inf.0)
) ;check-catch
(check-catch 'type-error
  (njson->string -inf.0)
) ;check-catch
(check-catch 'type-error
  (njson->string 1.0+2.0i)
) ;check-catch
(check (capture-type-error-message (lambda () (njson->string +nan.0))
       ) ;capture-type-error-message
  =>
  "g_njson-json->string: number must be finite (NaN/Inf are not valid JSON numbers)"
) ;check
(check (capture-type-error-message (lambda () (njson->string 1.0+2.0i))
       ) ;capture-type-error-message
  =>
  "g_njson-json->string: number must be real and finite"
) ;check
(check-catch 'type-error
  (njson->string 'foo)
) ;check-catch


(define njson-string-freed
  (string->njson "{\"k\":1}")
) ;define
(check-true (njson-free njson-string-freed)
) ;check-true
(check-catch 'type-error
  (njson->string njson-string-freed)
) ;check-catch


(check-report)
