(import (liii check) (liii string) (goldfish))

;; R7RS define-library `include' 回归：include 子句（顶层 + begin 内部）
;; 拼接进库体，随库展开并注册，值经运行时模块可查。

(let* ((form '(define-library (test lib-include-top)
                (export f h)
                (include "tests/resources/include-probe-inc.scm")))
       (stx (stx-set-library (wrap-expression form) the-base-library))
       (hdl (module-ref the-expander-library 'expand-define-library)))
  (let*-values (((defs ctx1) (hdl stx (initial-context))))
    (eval (cons 'begin (map lower defs)) (rootlet))
    (check ((module-ref '(test lib-include-top) 'f) 1) => 2)
    (check (module-ref '(test lib-include-top) 'h) => 7)))

(let* ((form '(define-library (test lib-include-begin)
                (export f g)
                (begin
                  (include "tests/resources/include-probe-inc.scm")
                  (define g 42))))
       (stx (stx-set-library (wrap-expression form) the-base-library))
       (hdl (module-ref the-expander-library 'expand-define-library)))
  (let*-values (((defs ctx1) (hdl stx (initial-context))))
    (eval (cons 'begin (map lower defs)) (rootlet))
    (check ((module-ref '(test lib-include-begin) 'f) 1) => 2)
    (check (module-ref '(test lib-include-begin) 'g) => 42)))

;; kernel 库（goldfish）经 load-library! / import 正式接入：
;; 其绑定即 base 库的活跃绑定，无需磁盘文件。
(check (procedure? expand) => #t)
(check (procedure? expand-library-body) => #t)
(check (procedure? syntax->datum) => #t)
(check (procedure? wrap-expression) => #t)
(load-library! '(goldfish))
(check (not (not (runtime-registered? '(goldfish)))) => #t)

(check-report)
