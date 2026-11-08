;; stream-unfold
;; 按展开函数从种子生成流。
;;
;; 语法
;; ----
;; (stream-unfold mapper pred? generator base)
;;
;; 参数
;; ----
;; mapper : procedure?
;; 种子到元素的映射。
;; pred? : procedure?
;; 继续展开条件。
;; generator : procedure?
;; 种子推进函数。
;; base : any
;; 初始种子。
;;
;; 返回值
;; ----
;; stream?
;; 满足 pred? 的连续种子经 mapper 映射的流。
;;
;; 说明
;; ----
;;
;; 错误处理
;; ----
;; type-error
;; mapper / pred? / generator 任一不是过程

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-unfold "four" odd? + 0))
(check-catch 'type-error (stream-unfold + "four" + 0))
(check-catch 'type-error (stream-unfold + odd? "four" 0))
(check
  (stream->list (stream-unfold (lambda (x) (* x x))
                  (lambda (x) (< x 10))
                  (lambda (x) (+ x 1))
                  0
                ) ;stream-unfold
  ) ;stream->list
  =>
  '(0 1 4 9 16 25 36 49 64 81)
) ;check

(check-report)
