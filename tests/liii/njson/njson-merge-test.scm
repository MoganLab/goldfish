(import (liii check)
  (liii base)
  (liii error)
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; njson-merge
;; 对象浅合并，返回新句柄，同名键由 source-json 覆盖。
;;
;; 语法
;; ----
;; (njson-merge target-json source-json)
;;
;; 参数
;; ----
;; target-json : njson-handle
;; source-json : njson-handle
;;
;; 返回值
;; ----
;; njson-handle
;; 合并后的新句柄。
;;
;; 注意
;; ----
;; 只接受 object <- object 合并，非 object 直接报 type-error。
;;
;; 错误处理
;; ----
;; type-error
;; target-json 或 source-json 不是可用的 njson object-handle 时抛出。


(define shallow-merge-base-json
  "{\"name\":\"base\",\"meta\":{\"x\":1},\"arr\":[1,2]}"
) ;define
(define shallow-merge-patch-json
  "{\"meta\":{\"y\":2},\"arr\":[9],\"extra\":true}"
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


(let-njson ((base (string->njson shallow-merge-base-json)
            ) ;base
            (patch (string->njson shallow-merge-patch-json)
            ) ;patch
            (merged (njson-merge base patch))
           ) ;
  (check (njson-ref merged "name")
    =>
    "base"
  ) ;check
  (check (njson-ref merged "extra") => #t)
  (check (njson-ref merged "meta" "y")
    =>
    2
  ) ;check
  (check-catch 'key-error
    (njson-ref merged "meta" "x")
  ) ;check-catch
  (check (njson-ref merged "arr" 0) => 9)
  (check (njson-size (njson-ref merged "arr"))
    =>
    1
  ) ;check
  (check (njson-ref base "meta" "x") => 1)
  (check-false (njson-contains-key? base "extra")
  ) ;check-false
) ;let-njson


(let-njson ((base (string->njson "{\"a\":1,\"nested\":{\"x\":2}}"
                  ) ;string->njson
            ) ;base
            (patch (string->njson "{\"a\":9,\"nested\":{\"y\":3}}"
                   ) ;string->njson
            ) ;patch
            (merged (njson-merge base patch))
           ) ;
  (check (njson-ref merged "a") => 9)
  (check (njson-ref merged "nested" "y")
    =>
    3
  ) ;check
  (check-catch 'key-error
    (njson-ref merged "nested" "x")
  ) ;check-catch
  (check (njson-ref base "nested" "x")
    =>
    2
  ) ;check
) ;let-njson


(let-njson ((base (string->njson "{\"a\":1}")))
  (check-catch 'type-error
    (njson-merge base 'foo)
  ) ;check-catch
  (check-catch 'type-error
    (njson-merge base 1)
  ) ;check-catch
  (check (capture-type-error-message (lambda () (njson-merge base 1))
         ) ;capture-type-error-message
    =>
    "njson-merge: source-json must be njson object-handle"
  ) ;check
) ;let-njson


(check-catch 'type-error
  (njson-merge 'foo 'null)
) ;check-catch


(define njson-merge-freed
  (string->njson "{\"a\":1}")
) ;define
(check-true (njson-free njson-merge-freed)
) ;check-true
(let-njson ((patch (string->njson "{\"b\":2}")))
  (check-catch 'type-error
    (njson-merge njson-merge-freed patch)
  ) ;check-catch
) ;let-njson


(check-report)
