;; stream-unfolds
;; 从单一种子同时展开多个流。
;;
;; 语法
;; ----
;; (stream-unfolds gen seed)
;;
;; 参数
;; ----
;; gen : procedure?
;; 返回 (next result ...)；result 为 (value)、#f 或 ()。
;; seed : any
;; 初始种子。
;;
;; 返回值
;; ----
;; 多个值
;; 每个结果槽一个流（values 返回）。
;;
;; 说明
;; ----
;; result 约定：(value) 提供元素、#f 跳过、() 终止该流。
;;
;; 错误处理
;; ----
;; type-error
;; gen 不是过程
;; value-error
;; gen 返回的结果槽不满足 (value) / #f / () 约定

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check
  (stream->list
    (stream-unfolds
      (lambda (x)
        (let ((n (car x)) (s (cdr x)))
          (if (zero? n)
            (values 'dummy '())
            (values (cons (- n 1) (stream-cdr s)) (list (stream-car s)))
          ) ;if
        ) ;let
      ) ;lambda
      (cons 5 (stream-from 0))
    ) ;stream-unfolds
  ) ;stream->list
  =>
  '(0 1 2 3 4)
) ;check
(let-values (((a b)
              (stream-unfolds
                (lambda (seed)
                  (if (= seed 4)
                    (values (+ seed 1) '() '())
                    (values (+ seed 1) (list seed) (list (* 2 seed)))
                  ) ;if
                ) ;lambda
                0
              ) ;stream-unfolds
             ) ;
            ) ;
  (check (stream->list a) => '(0 1 2 3))
  (check (stream->list b) => '(0 2 4 6))
) ;let-values

(check-report)
