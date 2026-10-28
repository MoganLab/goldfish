;; cond 值聚合测试的原库。
;; 依赖签名：my-assoc (any? alist? -> (or #f pair?))、
;; my-cdr (pair? -> any?)、my-cons (any? any? -> pair?)、
;; my-mul (number? number? -> number?)。
;; 用例形态取自真实代码：
;;   date-week.scm: (cond ... -1) (... 1) (else 0) 聚合后参与算术

(define-library (liii aggcheck)
  (import (liii reexport-dep))
  (export cond-ok cond-bad cond-no-else cond-arrow nested-ok nested-bad)
  (begin

    ;; cond：各子句末表达式聚合为 integer? -> 合法
    (define (cond-ok x)
      (my-mul
        (cond ((my-assoc 'a x) 1) ((my-assoc 'b x) 2) (else 3))
        2
      ) ;my-mul
    ) ;define

    ;; cond：else 子句返回 "s" -> (or integer? string?) 传 number? 冲突
    (define (cond-bad x)
      (my-mul
        (cond ((my-assoc 'a x) 1) (else "s"))
        2
      ) ;my-mul
    ) ;define

    ;; cond：无 else -> 可能落到假值 -> 传 number? 冲突
    (define (cond-no-else x)
      (my-mul
        (cond ((my-assoc 'a x) 1))
        2
      ) ;my-mul
    ) ;define

    ;; cond：=> 子句保守 unknown -> 放行
    (define (cond-arrow x)
      (my-mul
        (cond ((my-assoc 'a x) => my-cdr) (else 0))
        2
      ) ;my-mul
    ) ;define

    ;; 嵌套：cond 整体作为值传给 pair? 参数 -> 合法
    (define (nested-ok x)
      (my-cdr
        (cond
          ((my-assoc 'a x) (my-cons 1 2))
          (else (my-cons 3 4))
        ) ;cond
      ) ;my-cdr
    ) ;define

    ;; 嵌套：else 子句 "s" -> (or pair? string?) 传 pair? 冲突
    (define (nested-bad x)
      (my-cdr
        (cond
          ((my-assoc 'a x) (my-cons 1 2))
          (else "s")
        ) ;cond
      ) ;my-cdr
    ) ;define
  ) ;begin
) ;define-library
