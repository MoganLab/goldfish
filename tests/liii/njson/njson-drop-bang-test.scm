(import (liii check)
  (liii base)
  (liii error)
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; njson-drop!
;; 原地删除 JSON 路径，并返回同一句柄。
;;
;; 语法
;; ----
;; (njson-drop! json key ...)
;;
;; 参数
;; ----
;; json : njson-handle
;; key ... : string | integer
;;
;; 返回值
;; ----
;; njson-handle
;; 删除后的原句柄。
;;
;; 注意
;; ----
;; 删除成功后原句柄继续可用，但内容会更新。
;;
;; 错误处理
;; ----
;; type-error
;; json 非句柄或句柄已释放时抛出。
;; key-error
;; 路径不存在、token 类型不匹配或缺少路径参数时抛出。


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
  (njson-drop! root "active")
  (check-false (njson-contains-key? root "active")
  ) ;check-false
) ;let-njson


(let-njson ((arr (string->njson "[10,20,30]")))
  (njson-drop! arr 1)
  (check (njson-ref arr 0) => 10)
  (check (njson-ref arr 1) => 30)
  (check (njson-size arr) => 2)
) ;let-njson


(check-catch 'type-error
  (njson-drop! 'foo "active")
) ;check-catch
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error
    (njson-drop! root 'active)
  ) ;check-catch
  (check-catch 'key-error
    (njson-drop! root "not-found")
  ) ;check-catch
  (check-catch 'key-error
    (njson-drop! root "meta" "not-found")
  ) ;check-catch
  (check (capture-key-error-message (lambda ()
                                      (njson-drop! root "meta" "not-found")
                                    ) ;lambda
         ) ;capture-key-error-message
    =>
    "g_njson-drop!: path not found: missing object key 'not-found'"
  ) ;check
) ;let-njson
(let-njson ((arr (string->njson "[10,20,30]")))
  (check-catch 'key-error
    (njson-drop! arr 3)
  ) ;check-catch
  (check (capture-key-error-message (lambda () (njson-drop! arr 3))
         ) ;capture-key-error-message
    =>
    "g_njson-drop!: path not found: array index out of range (index=3, size=3)"
  ) ;check
) ;let-njson


(check-report)
