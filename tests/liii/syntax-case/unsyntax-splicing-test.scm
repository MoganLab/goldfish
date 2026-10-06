(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; unsyntax-splicing
;; 在 quasisyntax 准引用语法模板内部求值表达式，并将求值得到的列表元素平铺拼接嵌入模板。
;;
;; 语法
;; ----
;; (unsyntax-splicing list-expr)
;;
;; 参数
;; ----
;; list-expr : list
;; 求值结果必须为列表的表达式。
;;
;; 返回值
;; -----
;; list
;; 解包后的元素序列。

;; 1. 列表解包嵌入
(check (quasisyntax (list 0 (unsyntax-splicing (list 1 2 3)) 4))
       => '(list 0 1 2 3 4))

;; 2. 空列表解包嵌入
(check (quasisyntax (begin (unsyntax-splicing '()) #t))
       => '(begin #t))

;; 3. 动态列表计算与平铺
(check (quasisyntax (+ (unsyntax-splicing (map (lambda (x) (* x 2)) '(1 2 3)))))
       => '(+ 2 4 6))

(check-report)
