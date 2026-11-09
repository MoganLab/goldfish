;; walk-bodies
;; 在给定类型环境下遍历函数体表达式列表，收集实参类型冲突。
;;
;; 语法
;; ----
;; (walk-bodies rel bodies env acc)
;;
;; 参数
;; ----
;; rel : string          违规报告使用的相对路径
;; bodies : list         表达式列表
;; env : alist           类型环境（函数名 -> 签名列表）
;; acc : list            已有违规列表
;;
;; 返回值
;; ------
;; list of violation
;; 追加新违规后的列表（逆序语义，供 walk-defines 反转）。

(import (scheme base) (liii check) (liii goldcheck type-hint infer))

(check-set-mode! 'report-failed)

(define env
  (list (cons 'my-cdr '((pair? -> any?)))
        (cons 'my-assoc '((any? alist? -> (or #f pair?))))
       ) ;list
) ;define

;; 1. 确定冲突被报告：my-cdr 收到 (or #f pair?)
(let ((viols (walk-bodies "demo.scm"
               (list '(my-cdr (my-assoc 'a '())))
               env
               '()
             ) ;walk-bodies
       ) ;let
     ) ;
  (check (length viols) => 1)
) ;let

;; 2. 无冲突返回原 acc
(check (walk-bodies "demo.scm" (list '(my-cdr '(1 . 2))) env '()) => '())

;; 3. 空体返回原 acc
(check (walk-bodies "demo.scm" '() env '(pre)) => '(pre))

(check-report)
