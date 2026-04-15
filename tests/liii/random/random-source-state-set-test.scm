(import (liii random) (liii check))


(check-set-mode! 'report-failed)


;; random-source-state-set!
;; 设置随机源的状态。
;;
;; 语法
;; ----
;; (random-source-state-set! s state)
;;
;; 参数
;; ----
;; s : random-source
;; 要设置状态的随机源。
;;
;; state : list
;; 状态列表，格式为 (random-source-state seed carry)。
;;
;; 示例
;; ----
;; (let ((s (make-random-source)))
;;   (random-source-state-set! s '(random-source-state 123 456)))
;;
;; 错误处理
;; ----
;; wrong-type-arg 当 s 不是随机源或 state 格式无效时抛出。


(let ((s (make-random-source)))
  (let ((saved-state (random-source-state-ref s)
        ) ;saved-state
       ) ;
    (let ((rand-int (random-source-make-integers s)
          ) ;rand-int
         ) ;
      (rand-int 100)
      (rand-int 100)
      (random-source-state-set! s saved-state)
      (let ((reset-state (random-source-state-ref s)
            ) ;reset-state
           ) ;
        (check (equal? saved-state reset-state)
          =>
          #t
        ) ;check
      ) ;let
    ) ;let
  ) ;let
) ;let


(check-catch 'wrong-type-arg
  (random-source-state-set! 'not-a-source
    '(random-source-state 0 0)
  ) ;random-source-state-set!
) ;check-catch


(let ((s (make-random-source)))
  (check-catch 'wrong-type-arg
    (random-source-state-set! s
      'invalid-state
    ) ;random-source-state-set!
  ) ;check-catch
  (check-catch 'wrong-type-arg
    (random-source-state-set! s
      '(not-the-right-tag 0 0)
    ) ;random-source-state-set!
  ) ;check-catch
  (check-catch 'wrong-type-arg
    (random-source-state-set! s
      '(random-source-state)
    ) ;random-source-state-set!
  ) ;check-catch
) ;let


(check-report)
