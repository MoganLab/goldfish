;; packrat-parser
;; 定义 packrat 解析规则，生成带记忆化的解析组合子。
;;
;; 语法
;; ----
;; (packrat-parser nonterminal rule ...)
;;
;; 参数
;; ----
;; nonterminal : symbol
;; 非终结符名，作为记忆化缓存的键。
;; rule ... : any
;; 解析规则，每个规则形如 ((绑定 . '记号) 动作) 或 ((绑定 . 非终结符) 动作)。
;;
;; 返回值
;; ----
;; procedure
;; 接收 <parse-results> 并返回 <parse-result> 的解析组合子。
;;
;; 说明
;; ----
;; 1. 规则中用 <- 绑定记号或非终结符的解析结果，
;;    非终结符结果以记忆化方式缓存，避免重复解析。
;; 2. 支持 (^ 绑定) 绑定当前位置，以及 (! '记号) 断言记号。
;; 3. 本宏展开为 packrat-parser 语法糖，展开结果依赖 (liii match)。
;;
;; 错误处理
;; ----
;; 规则形状非法时在宏展开期抛出 type-error。

(import (liii check) (liii hash-table) (liii packrat) (scheme eval))

(check-set-mode! 'report-failed)

(define (generator tokens)
  (let ((stream tokens))
    (lambda ()
      (if (null? stream)
        (values #f #f)
        (let ((base-token (car stream)))
          (set! stream (cdr stream))
          (values #f base-token)
        ) ;let
      ) ;if
    ) ;lambda
  ) ;let
) ;define

;; 简单规则：匹配 num 或 id
(define simple-parser
  (packrat-parser expr (expr ((a <- 'num) a) ((a <- 'id) a)))
) ;define
(check-true (procedure? simple-parser))

(let* ((gen-num (generator '((num . 123))))
       (r-num (simple-parser (base-generator->results gen-num)))
      ) ;
  (check-true (parse-result-successful? r-num))
  (check (parse-result-semantic-value r-num) => 123)
) ;let*

(let* ((gen-id (generator '((id . foo))))
       (r-id (simple-parser (base-generator->results gen-id)))
      ) ;
  (check-true (parse-result-successful? r-id))
  (check (parse-result-semantic-value r-id) => 'foo)
) ;let*

(let* ((gen-invalid (generator '((foo . bar))))
       (r-invalid (simple-parser (base-generator->results gen-invalid)))
      ) ;
  (check-false (parse-result-successful? r-invalid))
) ;let*

;; 完整算术表达式解析器（右递归，packrat 不支持左递归）
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
    (powexp ((a <- simple '^ b <- powexp) (expt a b)) ((a <- simple) a))
    (simple ((a <- 'num) a)
     ((a <- 'id) (calc-env a))
     (('oparen a <- expr 'cparen) a)
    ) ;simple
    (exprs ((a <- expr rest <- exprs) rest) ((a <- expr) a))
  ) ;packrat-parser
) ;define
(check-true (procedure? calc))

(let* ((g (generator '((num . 2) (+) (num . 3))))
       (expected (+ 2 3))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

(let* ((g (generator '((num . 1) (-) (num . 2) (+) (num . 3))))
       (expected (- 1 (+ 2 3)))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

(let* ((g (generator '((num . 1) (*) (num . 2) (/) (num . 3))))
       (expected (* 1 (/ 2 3)))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

(let* ((g (generator '((oparen) (num . 2) (+) (num . 3) (cparen) (*) (num . 4))))
       (expected (* (+ 2 3) 4))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

(let* ((g (generator '((num . 2) (^) (num . 3))))
       (expected (expt 2 3))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

(let* ((g (generator '((num . 8) (/) (num . 2))))
       (expected (/ 8 2))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

;; 赋值与变量引用
(let* ((g (generator '((begin)
                       (id . ans)
                       (:=)
                       (num . 42)
                       (oparen)
                       (num . 2)
                       (+)
                       (id . ans)
                       (cparen)
                       (^)
                       (num . 3)
                       (end))
          ) ;generator
       ) ;g
       (expected (begin (define ans 42) (expt (+ 2 ans) 3)))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

(let* ((g (generator '((oparen)
                       (num . 2)
                       (+)
                       (num . 3)
                       (cparen)
                       (^)
                       (oparen)
                       (num . 1)
                       (+)
                       (num . 1)
                       (cparen))
          ) ;generator
       ) ;g
       (expected (expt (+ 2 3) (+ 1 1)))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

(let* ((g (generator '((begin)
                       (id . a)
                       (:=)
                       (num . 10)
                       (id . b)
                       (:=)
                       (num . 20)
                       (id . a)
                       (*)
                       (id . b)
                       (end))
          ) ;generator
       ) ;g
       (expected (begin (define a 10) (define b 20) (* a b)))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

(let* ((g-invalid (generator '((begin) (foo . bar) (end))))
       (r-invalid (calc (base-generator->results g-invalid)))
      ) ;
  (check-false (parse-result-successful? r-invalid))
) ;let*
(hash-table-clear! calc-env)

;; 位置绑定 (^ pos)
(let ()
  (define (pos-generator tokens)
    (let ((g (generator tokens)) (col 1))
      (lambda ()
        (let-values (((_ tok) (g)))
          (let ((pos (make-parse-position "test" 1 col)))
            (if tok (set! col (+ col 1)))
            (values pos tok)
          ) ;let
        ) ;let-values
      ) ;lambda
    ) ;let
  ) ;define
  (define p-pos
    (packrat-parser entry
      (entry ((pos <- ^ x <- 'num) (list (parse-position-line pos) (parse-position-column pos) x)))
    ) ;packrat-parser
  ) ;define
  (let* ((g (pos-generator '((num . 42))))
         (r (p-pos (base-generator->results g)))
        ) ;
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => '(1 1 42))
  ) ;let*
) ;let

;; 否定断言 (! 'bad)
(let ()
  (define p-not
    (packrat-parser entry
      (entry (((! 'bad) x <- 'id) x))
    ) ;packrat-parser
  ) ;define
  (let* ((g1 (generator '((id . foo))))
         (r1 (p-not (base-generator->results g1)))
        ) ;
    (check-true (parse-result-successful? r1))
    (check (parse-result-semantic-value r1) => 'foo)
  ) ;let*
  (let* ((g2 (generator '((bad . 1) (id . foo))))
         (r2 (p-not (base-generator->results g2)))
        ) ;
    (check-false (parse-result-successful? r2))
  ) ;let*
) ;let

;; 未绑定记号与带引号的未绑定记号
(let ()
  (define p-unbound
    (packrat-parser entry
      (entry ((skip x <- 'num) x))
      (skip (('tag) 'done))
    ) ;packrat-parser
  ) ;define
  (let* ((g (generator '((tag . #t) (num . 99))))
         (r (p-unbound (base-generator->results g)))
        ) ;
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => 99)
  ) ;let*
) ;let

(let ()
  (define p-quote-unbound
    (packrat-parser entry
      (entry (('tag x <- 'num) x))
    ) ;packrat-parser
  ) ;define
  (let* ((g (generator '((tag . #t) (num . 88))))
         (r (p-quote-unbound (base-generator->results g)))
        ) ;
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => 88)
  ) ;let*
) ;let

;; 非法规则在宏展开期报错
(check-catch 'type-error
  (eval '(packrat-parser entry (entry (#t 1))) (environment '(liii packrat)))
) ;check-catch

(check-report)
