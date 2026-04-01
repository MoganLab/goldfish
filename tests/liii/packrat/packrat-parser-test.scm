(import (liii check)
        (liii hash-table)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; packrat-parser
;; 创建基于 packrat 的完整解析器。
;;
;; 语法
;; ----
;; (packrat-parser result-expr nonterminal-def ...)
;;
;; 参数
;; ----
;; result-expr : any
;;   结果表达式，作为最终返回的解析器
;; nonterminal-def : list
;;   非终结符定义列表
;;
;; 返回值
;; ----
;; procedure
;;   完整解析器过程
;;
;; 描述
;; ----
;; packrat-parser 是创建解析器的核心宏，支持递归下降解析
;; 和记忆化。非终结符定义使用 PEG 风格的语法。
;; 注意：packrat 解析器不支持左递归。

(define (generator tokens)
  (let ((stream tokens))
    (lambda ()
      (if (null? stream)
          (values #f #f)
          (let ((token (car stream)))
            (set! stream (cdr stream))
            (values #f token)
          ) ;let
      ) ;if
    ) ;lambda
  ) ;let
) ;define

;; 示例1：简单解析器
(let ()
  (define simple-parser
    (packrat-parser expr
      (expr ((a <- 'num) a)
            ((a <- 'id) a)
      ) ;expr
    ) ;packrat-parser
  ) ;define
  (check-true (procedure? simple-parser))

  (let* ((gen-num (generator '((num . 123))))
         (r-num (simple-parser (base-generator->results gen-num))))
    (check-true (parse-result-successful? r-num))
    (check (parse-result-semantic-value r-num) => 123)
  ) ;let*

  (let* ((gen-id (generator '((id . foo))))
         (r-id (simple-parser (base-generator->results gen-id))))
    (check-true (parse-result-successful? r-id))
    (check (parse-result-semantic-value r-id) => 'foo)
  ) ;let*

  (let* ((gen-invalid (generator '((foo . bar))))
         (r-invalid (simple-parser (base-generator->results gen-invalid))))
    (check-false (parse-result-successful? r-invalid))
  ) ;let*
) ;let

;; 示例2：计算器解析器
(let ()
  (define calc-env (make-hash-table))
  (define calc
    (packrat-parser expr
      (expr (('begin body <- exprs 'end) body)
            ((var <- 'id ':= val <- expr) (hash-table-set! calc-env var val))
            ((a <- mulexp '+ b <- expr) (+ a b))
            ((a <- mulexp '- b <- expr) (- a b))
            ((a <- mulexp) a)
      ) ;expr
      (mulexp ((a <- powexp '* b <- mulexp) (* a b))
              ((a <- powexp '/ b <- mulexp) (/ a b))
              ((a <- powexp) a)
      ) ;mulexp
      (powexp ((a <- simple '^ b <- powexp) (expt a b))
              ((a <- simple) a)
      ) ;powexp
      (simple ((a <- 'num) a)
              ((a <- 'id) (calc-env a))
              (('oparen a <- expr 'cparen) a)
      ) ;simple
      (exprs ((a <- expr rest <- exprs) rest)
             ((a <- expr) a)
      ) ;exprs
    ) ;packrat-parser
  ) ;define
  (check-true (procedure? calc))

  ;; 加法测试
  (let* ((g (generator '((num . 2) (+) (num . 3))))
         (expected (+ 2 3))
         (r (calc (base-generator->results g))))
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => expected)
  ) ;let*
  (hash-table-clear! calc-env)

  ;; 右递归减法测试
  (let* ((g (generator '((num . 1) (-) (num . 2) (+) (num . 3))))
         (expected (- 1 (+ 2 3)))
         (r (calc (base-generator->results g))))
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => expected)
  ) ;let*
  (hash-table-clear! calc-env)

  ;; 乘除法测试
  (let* ((g (generator '((num . 1) (*) (num . 2) (/) (num . 3))))
         (expected (* 1 (/ 2 3)))
         (r (calc (base-generator->results g))))
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => expected)
  ) ;let*
  (hash-table-clear! calc-env)

  ;; 括号测试
  (let* ((g (generator '((oparen) (num . 2) (+) (num . 3) (cparen)
                         (*) (num . 4))))
         (expected (* (+ 2 3) 4))
         (r (calc (base-generator->results g))))
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => expected)
  ) ;let*
  (hash-table-clear! calc-env)

  ;; 幂运算测试
  (let* ((g (generator '((num . 2) (^) (num . 3))))
         (expected (expt 2 3))
         (r (calc (base-generator->results g))))
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => expected)
  ) ;let*
  (hash-table-clear! calc-env)

  ;; 变量赋值测试
  (let* ((g (generator
             '((begin) (id . ans) (:=) (num . 42)
               (oparen) (num . 2) (+) (id . ans) (cparen)
               (^) (num . 3)
               (end))))
         (expected (begin (define ans 42)
                          (expt (+ 2 ans) 3))
         ) ;expected
         (r (calc (base-generator->results g))))
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => expected)
  ) ;let*
  (hash-table-clear! calc-env)

  ;; 多变量赋值测试
  (let* ((g (generator '((begin) (id . a) (:=) (num . 10)
                         (id . b) (:=) (num . 20)
                         (id . a) (*) (id . b)
                         (end))))
         (expected (begin (define a 10) (define b 20) (* a b)))
         (r (calc (base-generator->results g))))
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => expected)
  ) ;let*
  (hash-table-clear! calc-env)

  ;; 无效输入测试
  (let* ((g-invalid (generator '((begin) (foo . bar) (end))))
         (r-invalid (calc (base-generator->results g-invalid))))
    (check-false (parse-result-successful? r-invalid))
  ) ;let*
  (hash-table-clear! calc-env)
) ;let

(check-report)
