(import (liii check)
  (liii base)
  (liii error)
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; let-njson
;; 在作用域退出时自动释放 njson 句柄，并允许普通值直接透传。
;;
;; 语法
;; ----
;; (let-njson (binding ...) body ...)
;;
;; 参数
;; ----
;; binding : (var value)
;; 绑定变量与值，值可以是 njson 句柄或普通 Scheme 值。
;;
;; 返回值
;; ----
;; any?
;; 返回 body 的最后一个表达式结果。
;;
;; 注意
;; ----
;; 绑定到 njson 句柄时，离开作用域会自动 free。
;;
;; 错误处理
;; ----
;; type-error
;; 绑定语法非法或空绑定列表时抛出。


(check-catch 'type-error
  (let-njson ((j (string->njson 1))) j)
) ;check-catch


(define let-njson-cleanup '())
(check (let-njson ((j (string->njson "{\"name\":\"Goldfish\"}"
                      ) ;string->njson
                   ) ;j
                  ) ;
         (set! let-njson-cleanup j)
         (njson-ref j "name")
       ) ;let-njson
  =>
  "Goldfish"
) ;check
(check-catch 'type-error
  (njson-ref let-njson-cleanup "name")
) ;check-catch


(define let-njson-multi-a '())
(define let-njson-multi-b '())
(check (let-njson ((j1 (string->njson "{\"a\":1}"))
                   (j2 (string->njson "{\"b\":2}"))
                  ) ;
         (set! let-njson-multi-a j1)
         (set! let-njson-multi-b j2)
         (+ (njson-ref j1 "a")
           (njson-ref j2 "b")
         ) ;+
       ) ;let-njson
  =>
  3
) ;check
(check-catch 'type-error
  (njson-ref let-njson-multi-a "a")
) ;check-catch
(check-catch 'type-error
  (njson-ref let-njson-multi-b "b")
) ;check-catch


(check (let-njson ((x 7) (y 5)) (+ x y))
  =>
  12
) ;check


(check-catch 'type-error
  (let-njson () #t)
) ;check-catch


(check-report)
