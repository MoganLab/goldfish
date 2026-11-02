;; walk-cond
;; 遍历 cond 控制流表达式，各分支按其测试条件为真收窄局部变量类型。
;;
;; 语法
;; ----
;; (walk-cond walk walk-body rel expr env locals acc)
;;
;; 参数
;; ----
;; walk : procedure       遍历单个表达式的递归函数
;; walk-body : procedure  遍历分支体表达式列表的递归函数
;; rel : string           相对文件路径
;; expr : pair            cond 表达式 AST
;; env : any              环境
;; locals : list          局部变量类型表
;; acc : list             违规累加器
;;
;; 返回值
;; ------
;; list
;; 累加器结果。

(import (scheme base) (liii check) (liii goldcheck type-hint narrow))

(check-set-mode! 'report-failed)

(let* ((seen-body '())
       (mock-walk (lambda (rel e env locals acc) acc))
       (mock-walk-body
         (lambda (rel body env locals acc)
           (set! seen-body (cons (cons body locals) seen-body))
           acc
         ) ;lambda
       ) ;mock-walk-body
       (expr '(cond ((string? x) (do-string x))))
       (locals '((x or #f string?)))
      ) ;
  (walk-cond mock-walk mock-walk-body "a.scm" expr #f locals '())
  ;; 分支体执行时 x 应当被收窄为 string?
  (let ((body-locals (cdar seen-body)))
    (check (cdr (assq 'x body-locals)) => 'string?)
  ) ;let
) ;let*

(check-report)
