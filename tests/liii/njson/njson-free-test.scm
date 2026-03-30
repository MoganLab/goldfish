(import (liii check)
        (liii base)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-free
;; 释放一个 njson 句柄，并使后续访问失效。
;;
;; 语法
;; ----
;; (njson-free handle)
;;
;; 参数
;; ----
;; handle : njson-handle
;; 待释放的句柄。
;;
;; 返回值
;; ----
;; boolean?
;; 释放成功返回真。
;;
;; 注意
;; ----
;; 释放后再次访问同一句柄会触发 type-error。
;;
;; 错误处理
;; ----
;; type-error
;; 输入不是可用 njson 句柄时抛出。

(define free-check (string->njson "{\"x\":1}"))
(check-true (njson-free free-check))
(check-catch 'type-error (njson-ref free-check "x"))
(check-catch 'type-error (njson-free 'foo))

(define stale-handle-old (string->njson "{\"a\":1}"))
(check (njson-ref stale-handle-old "a") => 1)
(check-true (njson-free stale-handle-old))
(let-njson ((stale-handle-new (string->njson "{\"b\":2}")))
  (check (njson-ref stale-handle-new "b") => 2)
  (check-catch 'type-error (njson-ref stale-handle-old "b"))
  (check-catch 'type-error (njson-free stale-handle-old))
  (check (njson-ref stale-handle-new "b") => 2)
) ;let-njson

(check-catch 'type-error (njson-ref (cons 'njson-handle 1) "x"))

(let-njson ((root (string->njson "{\"secret\":42}")))
  (let* ((payload (cdr root))
         (id (car payload))
         (gen (cdr payload))
         (forged (cons 'njson-handle (cons id (+ gen 1)))))
    (check-catch 'type-error (njson-ref forged "secret"))
  ) ;let*
) ;let-njson

(check-report)
