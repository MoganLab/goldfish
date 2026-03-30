(import (liii check)
        (liii base)
        (liii error)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-merge!
;; 对象浅合并，原地修改 target-json。
;;
;; 语法
;; ----
;; (njson-merge! target-json source-json)
;;
;; 参数
;; ----
;; target-json : njson-handle
;; source-json : njson-handle
;;
;; 返回值
;; ----
;; njson-handle
;; 返回更新后的原句柄。
;;
;; 注意
;; ----
;; 合并成功后，njson-keys 缓存会失效并重建。
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

(define (string-list-contains? s xs)
  (cond ((null? xs) #f)
        ((string=? s (car xs)) #t)
        (else (string-list-contains? s (cdr xs)))
  ) ;cond
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

(let-njson ((base (string->njson shallow-merge-base-json))
            (patch (string->njson shallow-merge-patch-json)))
  (check-true (njson? (njson-merge! base patch)))
  (check (njson-ref base "meta" "y") => 2)
  (check-catch 'key-error (njson-ref base "meta" "x"))
  (check (njson-ref base "arr" 0) => 9)
  (check (njson-size (njson-ref base "arr")) => 1)
  (check-true (njson-contains-key? base "extra"))
) ;let-njson

(let-njson ((base (string->njson "{\"k\":1,\"left\":true}"))
            (patch (string->njson "{\"k\":9,\"new-key\":2}")))
  (check-true (string-list-contains? "k" (njson-keys base)))
  (check-false (string-list-contains? "new-key" (njson-keys base)))
  (check-true (njson? (njson-merge! base patch)))
  (let ((keys (njson-keys base)))
    (check-true (string-list-contains? "k" keys))
    (check-true (string-list-contains? "new-key" keys))
  ) ;let
  (check (njson-ref base "k") => 9)
) ;let-njson

(let-njson ((base (string->njson "{\"a\":1,\"nested\":{\"x\":2}}")))
  (check-true (njson? (njson-merge! base base)))
  (check (njson-ref base "a") => 1)
  (check (njson-ref base "nested" "x") => 2)
) ;let-njson

(let-njson ((base (string->njson "{\"a\":1}")))
  (check-catch 'type-error (njson-merge! base 'foo))
  (check-catch 'type-error (njson-merge! base 1))
  (check (capture-type-error-message (lambda () (njson-merge! base 1)))
         => "njson-merge!: source-json must be njson object-handle"
  ) ;check
) ;let-njson

(check-catch 'type-error (njson-merge! 'foo 'null))

(define njson-merge!-freed (string->njson "{\"a\":1}"))
(check-true (njson-free njson-merge!-freed))
(let-njson ((patch (string->njson "{\"b\":2}")))
  (check-catch 'type-error (njson-merge! njson-merge!-freed patch))
) ;let-njson

(check-report)
