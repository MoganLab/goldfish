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
;; type-error 当 s 不是随机源时抛出。


(let ((s (make-random-source)))
  (let ((state1 (random-source-state-ref s)))
    (random-source-randomize! s)
    (let ((state2 (random-source-state-ref s)))
      (check (not (equal? state1 state2)) => #t)
    ) ;let
  ) ;let
) ;let


(let ((s (make-random-source)))
  (random-source-randomize! s)
  (let ((state1 (random-source-state-ref s)))
    (random-source-randomize! s)
    (let ((state2 (random-source-state-ref s)))
      (check (not (equal? state1 state2)) => #t)
    ) ;let
  ) ;let
) ;let


(check-catch 'type-error (random-source-randomize! 'not-a-source))
(check-catch 'type-error (random-source-randomize! 123))

;; 验证两次独立 randomize! 产生的随机序列不同
(let* ((s1 (make-random-source)) (s2 (make-random-source)))
  (random-source-randomize! s1)
  (random-source-randomize! s2)
  (let ((gen1 (random-source-make-integers s1))
        (gen2 (random-source-make-integers s2))
       ) ;
    (let ((seq1 (list (gen1 1000) (gen1 1000) (gen1 1000) (gen1 1000) (gen1 1000)))
          (seq2 (list (gen2 1000) (gen2 1000) (gen2 1000) (gen2 1000) (gen2 1000)))
         ) ;
      (check (not (equal? seq1 seq2)) => #t)
    ) ;let
  ) ;let
) ;let*

;; 验证不引入 (srfi srfi-19)
(check (*r7rs-libraries* '(srfi srfi-19)) => #f)
(check (defined? 'TIME-UTC) => #f)

(check-report)
