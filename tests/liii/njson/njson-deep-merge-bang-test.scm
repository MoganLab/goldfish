(import (liii check)
        (liii base)
        (liii error)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-deep-merge!
;; 对象深合并，原地修改 target-json。
;;
;; 语法
;; ----
;; (njson-deep-merge! target-json source-json)
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

(define deep-merge-base-json
  "{\"name\":\"base\",\"meta\":{\"x\":1,\"nested\":{\"a\":1}},\"arr\":[1,2],\"override\":{\"k\":1}}"
) ;define
(define deep-merge-patch-json
  "{\"meta\":{\"y\":2,\"nested\":{\"b\":2}},\"arr\":[9],\"override\":0}"
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

(let-njson ((base (string->njson deep-merge-base-json))
            (patch (string->njson deep-merge-patch-json)))
  (check-true (njson? (njson-deep-merge! base patch)))
  (check (njson-ref base "meta" "x") => 1)
  (check (njson-ref base "meta" "y") => 2)
  (check (njson-ref base "meta" "nested" "a") => 1)
  (check (njson-ref base "meta" "nested" "b") => 2)
  (check (njson-ref base "override") => 0)
) ;let-njson

(let-njson ((base (string->njson "{\"meta\":{\"x\":1}}"))
            (patch (string->njson "{\"meta\":{\"y\":2},\"new-top\":1}")))
  (check-true (string-list-contains? "meta" (njson-keys base)))
  (check-false (string-list-contains? "new-top" (njson-keys base)))
  (check-true (njson? (njson-deep-merge! base patch)))
  (let ((keys (njson-keys base)))
    (check-true (string-list-contains? "meta" keys))
    (check-true (string-list-contains? "new-top" keys))
  ) ;let
  (check (njson-ref base "meta" "x") => 1)
  (check (njson-ref base "meta" "y") => 2)
) ;let-njson

(let-njson ((base (string->njson "{\"meta\":{\"x\":1,\"nested\":{\"k\":1}}}")))
  (check-true (njson? (njson-deep-merge! base base)))
  (check (njson-ref base "meta" "x") => 1)
  (check (njson-ref base "meta" "nested" "k") => 1)
) ;let-njson

(let-njson ((base (string->njson "{\"a\":1}")))
  (check-catch 'type-error (njson-deep-merge! base 'foo))
  (check-catch 'type-error (njson-deep-merge! base 1))
  (check (capture-type-error-message (lambda () (njson-deep-merge! base 1)))
         => "njson-deep-merge!: source-json must be njson object-handle"
  ) ;check
) ;let-njson

(check-catch 'type-error (njson-deep-merge! 'foo 'null))

(define njson-deep-merge!-freed (string->njson "{\"a\":1}"))
(check-true (njson-free njson-deep-merge!-freed))
(let-njson ((patch (string->njson "{\"b\":2}")))
  (check-catch 'type-error (njson-deep-merge! njson-deep-merge!-freed patch))
) ;let-njson

(check-report)
