(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; cyclic-sequences
;; 返回对象内部所有环形（循环引用）子结构组成的列表。
;;
;; 语法
;; ----
;; (cyclic-sequences obj)
;;
;; 参数
;; ----
;; obj : any?
;; 要检查的对象。
;;
;; 返回值
;; ------
;; list?
;; 对象中所有环形子结构的列表；若不存在环形结构，返回空列表。
;;
;; 描述
;; ----
;; 1. 普通（无环）对象返回空列表。
;; 2. 环形列表（如尾部的 cdr 指回自身）会被检测出来。
;; 3. 嵌套结构中的环形子结构也会被检测出来。
;; 4. 常用于在打印、遍历前检测循环引用，避免无限递归。


;; 普通对象没有环形结构
(check (cyclic-sequences 42) => '())
(check (cyclic-sequences "hello") => '())
(check (cyclic-sequences '(1 2 3)) => '())
(check (cyclic-sequences #(1 2 3)) => '())


;; 环形列表：尾部的 cdr 指回自身
(let ((x (list 1 2 3)))
  (set-cdr! (cddr x) x)
  (check (pair? (cyclic-sequences x)) => #t)
  (check (car (cyclic-sequences x)) => x)
) ;let


;; 嵌套结构中的环形子结构
(let ((inner (list 'a 'b)))
  (set-cdr! (cdr inner) inner)
  (let ((outer (list 1 inner 3)))
    (check (pair? (cyclic-sequences outer)) => #t)
  ) ;let
) ;let


(check-report)
