;; letrec-syntax
;; 在局部词法作用域中引入支持递归引用的语法关键字绑定。
;;
;;
;; 语法
;; ----
;; (letrec-syntax ((<keyword> <transformer-spec>) ...) <body> ...)
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

(import (scheme base) (srfi 78))

(check-set-mode! 'report-failed)

;; 测试1：递归宏绑定
(check
  (letrec-syntax
    ((my-or
       (syntax-rules ()
         ((my-or) #f)
         ((my-or e) e)
         ((my-or e1 e2 ...) (if e1 e1 (my-or e2 ...)))
       ) ;syntax-rules
     ) ;my-or
    ) ;
    (my-or #f #f 42)
  ) ;letrec-syntax
  =>
  42
) ;check

;; 测试2：递归列表重构
(check
  (letrec-syntax
    ((my-reverse
       (syntax-rules ()
         ((my-reverse () acc) acc)
         ((my-reverse (head . tail) acc) (my-reverse tail (cons head acc)))
       ) ;syntax-rules
     ) ;my-reverse
    ) ;
    (my-reverse (1 2 3) '())
  ) ;letrec-syntax
  =>
  '(3 2 1)
) ;check

;; 测试3：卫生性——递归宏模板内的 let/if 不被 use-site 同名局部绑定捕获（#1290）
;; 模板 (let ((temp e1)) (if temp ... (my-or e2 ...))) 中的 let 与 if 必须引用
;; 定义处的特殊形式，而非使用点 (let odd?)/(if even?) 里的过程值；
;; temp 与 use-site 的 temp=8 也必须同名独立，互不捕获。
(check
  (letrec-syntax
    ((my-or
       (syntax-rules ()
         ((my-or) #f)
         ((my-or e) e)
         ((my-or e1 e2 ...) (let ((temp e1)) (if temp temp (my-or e2 ...))))
       ) ;syntax-rules
     ) ;my-or
    ) ;
    (let ((x #f) (y 7) (temp 8) (let odd?) (if even?))
      (my-or x (let temp) (if y) y)
    ) ;let
  ) ;letrec-syntax
  =>
  7
) ;check

(check-report)
