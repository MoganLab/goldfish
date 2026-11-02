;; infer-cond
;; 推导 cond 表达式各分支末尾返回值的联合类型（无 else 时并入 boolean? 假值）。
;;
;; 语法
;; ----
;; (infer-cond infer-type expr env locals)
;;
;; 参数
;; ----
;; infer-type : procedure  递归类型推导函数 (lambda (e env locals) ...)
;; expr : pair             cond 表达式 AST
;; env : any               符号类型环境
;; locals : list           局部变量类型映射
;;
;; 返回值
;; ------
;; symbol, list 或 #f
;; 分支类型的并集；存在未知类型时返回 #f。

(import (scheme base) (liii check) (liii goldcheck type-hint value))

(check-set-mode! 'report-failed)

(define (mock-infer e env locals)
  (cond ((integer? e) 'integer?)
        ((string? e) 'string?)
        (else #f)
  ) ;cond
) ;define

;; 1. 同类型分支聚合
(let* ((expr '(cond ((> x 0) 1) (else 2))) (t (infer-cond mock-infer expr #f '())))
  (check t => 'integer?)
) ;let*

;; 2. 不同类型联合
(let* ((expr '(cond ((> x 0) "hello") (else 1)))
       (t (infer-cond mock-infer expr #f '()))
      ) ;
  (check (or (equal? t '(or string? integer?)) (equal? t '(or integer? string?)))
    =>
    #t
  ) ;check
) ;let*

;; 3. 无 else 分支时并入 boolean?
(let* ((expr '(cond ((> x 0) 1))) (t (infer-cond mock-infer expr #f '())))
  (check (or (equal? t '(or boolean? integer?)) (equal? t '(or integer?
                                                             boolean?)))
    =>
    #t
  ) ;check
) ;let*

(check-report)
