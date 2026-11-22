;; let-syntax
;; 在局部词法作用域中引入语法关键字绑定。
;;
;;
;; 语法
;; ----
;; (let-syntax ((<keyword> <transformer-spec>) ...) <body> ...)
;;
;; 参数
;; ----
;; <keyword> : identifier
;; 局部语法关键字。
;;
;; <transformer-spec> : transformer
;; 宏转换器规格说明，如 (syntax-rules ...)。
;;
;; <body> ... : any
;; 在新语法环境中展开的一个或多个表达式序列。
;;
;; 返回值
;; ----
;; any
;; 返回最后一个 body 表达式的求值结果。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 测试1：基础局部宏绑定
(check
  (let-syntax
    ((add1 (syntax-rules () ((add1 x) (+ x 1)))))
    (add1 5)
  ) ;let-syntax
  =>
  6
) ;check

;; 测试2：多个局部宏互不相干展开
(check
  (let-syntax
    ((one (syntax-rules () ((one) 1))) (two (syntax-rules () ((two) 2))))
    (+ (one) (two))
  ) ;let-syntax
  =>
  3
) ;check

;; 测试3：卫生性——宏模板内的 if 不被 use-site 同名局部绑定捕获（#1290）
;; 模板展开生成的 (if test (begin ...)) 中的 if 必须引用定义处的特殊形式，
;; 而非使用点 (let ((if #t))) 里的布尔值。
(check
  (let-syntax
    ((when
       (syntax-rules ()
         ((when test stmt1 stmt2 ...) (if test (begin stmt1 stmt2 ...)))
       ) ;syntax-rules
     ) ;when
    ) ;
    (let ((if #t))
      (when if
        (set! if 'now)
      ) ;when
      if
    ) ;let
  ) ;let-syntax
  =>
  'now
) ;check

;; 测试4：卫生性——宏模板内的自由标识符引用定义点词法绑定（R7RS 4.3.1）
;; 模板里的 x 应指 let-syntax 定义处可见的 x（'outer），
;; 而不是使用点 (let ((x 'inner))) 里的 'inner。
(check
  (let ((x 'outer))
    (let-syntax
      ((m (syntax-rules () ((m) x))))
      (let ((x 'inner))
        (m)
      ) ;let
    ) ;let-syntax
  ) ;let
  =>
  'outer
) ;check

(check-report)
