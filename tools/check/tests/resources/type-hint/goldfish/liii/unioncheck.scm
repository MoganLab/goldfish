;; 期望类型为联合类型时的实参检查，以及关键字参数调用的保守跳过。

(define-library (liii unioncheck)
  (import (scheme base) (liii union-dep))
  (export bad-union ok-union bad-kw ok-kw)
  (begin

    ;; my-take 需要 #f 或字符串，传入整数 -> 确定冲突
    (define (bad-union x)
      (my-take 42)
    ) ;define

    ;; 字符串可被联合类型接受 -> 无冲突
    (define (ok-union x)
      (my-take "s")
    ) ;define

    ;; 关键字调用：实参与位置的对应关系不确定 -> 保守跳过
    (define (bad-kw x)
      (my-opts :flag 2)
    ) ;define

    ;; 位置调用：第 2 个实参需为布尔值 -> 确定冲突
    (define (ok-kw x)
      (my-opts #t 2)
    ) ;define
  ) ;begin
) ;define-library
