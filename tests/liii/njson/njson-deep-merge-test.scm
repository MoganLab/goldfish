(import (liii check)
        (liii base)
        (liii error)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-deep-merge
;; 对象深合并，返回新句柄，同名 object 递归合并，其他值由 source 覆盖。
;;
;; 语法
;; ----
;; (njson-deep-merge target-json source-json)
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
;; 只有 object 对 object 时才递归合并，array 仍整体替换。
;;
;; 错误处理
;; ----
;; type-error
;; target-json 或 source-json 不是可用的 njson object-handle 时抛出。

(define deep-merge-base-json
  "{\"name\":\"base\",\"meta\":{\"x\":1,\"nested\":{\"a\":1}},\"arr\":[1,2],\"override\":{\"k\":1}}"
) ;define
(define deep-merge-patch-json
  "{\"meta\":{\"y\":2,\"nested\":{\"b\":2}},\"arr\":[9],\"override\":0}"
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

(let-njson ((base (string->njson deep-merge-base-json))
            (patch (string->njson deep-merge-patch-json))
            (merged (njson-deep-merge base patch)))
  (check (njson-ref merged "meta" "x") => 1)
  (check (njson-ref merged "meta" "y") => 2)
  (check (njson-ref merged "meta" "nested" "a") => 1)
  (check (njson-ref merged "meta" "nested" "b") => 2)
  (check (njson-ref merged "arr" 0) => 9)
  (check (njson-size (njson-ref merged "arr")) => 1)
  (check (njson-ref merged "override") => 0)
  (check-catch 'key-error (njson-ref base "meta" "y"))
  (check-catch 'key-error (njson-ref base "meta" "nested" "b"))
  (check (njson-ref base "override" "k") => 1)
) ;let-njson

(let-njson ((base (string->njson "{\"k\":{\"a\":1},\"arr\":[1,2]}"))
            (patch (string->njson "{\"k\":{\"b\":2},\"arr\":[9,8]}"))
            (merged (njson-deep-merge base patch)))
  (check (njson-ref merged "k" "a") => 1)
  (check (njson-ref merged "k" "b") => 2)
  (check (njson-size (njson-ref merged "arr")) => 2)
  (check (njson-ref merged "arr" 0) => 9)
) ;let-njson

(let-njson ((base (string->njson "{\"meta\":{\"x\":1,\"nested\":{\"k\":1}}}")))
  (check (njson->string (njson-deep-merge base base)) => (njson->string base))
  (check (njson-ref base "meta" "nested" "k") => 1)
) ;let-njson

(let-njson ((base (string->njson "{\"a\":1}")))
  (check-catch 'type-error (njson-deep-merge base 'foo))
  (check-catch 'type-error (njson-deep-merge base 1))
  (check (capture-type-error-message (lambda () (njson-deep-merge base 1)))
         => "njson-deep-merge: source-json must be njson object-handle"
  ) ;check
) ;let-njson

(check-catch 'type-error (njson-deep-merge 'foo 'null))

(define njson-deep-merge-freed (string->njson "{\"a\":1}"))
(check-true (njson-free njson-deep-merge-freed))
(let-njson ((patch (string->njson "{\"b\":2}")))
  (check-catch 'type-error (njson-deep-merge njson-deep-merge-freed patch))
) ;let-njson

(check-report)
