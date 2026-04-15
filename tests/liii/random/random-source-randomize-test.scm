(import (liii random) (liii check))


(check-set-mode! 'report-failed)


;; random-source-randomize!
;; 使用当前时间随机化随机源的状态。
;;
;; 语法
;; ----
;; (random-source-randomize! s)
;;
;; 参数
;; ----
;; s : random-source
;; 要随机化的随机源。
;;
;; 返回值
;; ----
;; undefined
;; 副作用：修改 s 的状态。
;;
;; 示例
;; ----
;; (let ((s (make-random-source)))
;;   (random-source-randomize! s)
;;   s)  ; s 现在处于随机状态
;;
;; 错误处理
;; ----
;; wrong-type-arg 当 s 不是随机源时抛出。


(let ((s (make-random-source)))
  (let ((state1 (random-source-state-ref s)))
    (random-source-randomize! s)
    (let ((state2 (random-source-state-ref s)))
      (check (not (equal? state1 state2))
        =>
        #t
      ) ;check
    ) ;let
  ) ;let
) ;let


(let ((s (make-random-source)))
  (random-source-randomize! s)
  (let ((state1 (random-source-state-ref s)))
    (random-source-randomize! s)
    (let ((state2 (random-source-state-ref s)))
      (check (not (equal? state1 state2))
        =>
        #t
      ) ;check
    ) ;let
  ) ;let
) ;let


(check-catch 'wrong-type-arg
  (random-source-randomize! 'not-a-source)
) ;check-catch
(check-catch 'wrong-type-arg
  (random-source-randomize! 123)
) ;check-catch


(check-report)
