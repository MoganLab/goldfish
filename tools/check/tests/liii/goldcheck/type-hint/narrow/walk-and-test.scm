;; walk-and
;; 遍历 and 控制流表达式，后置操作数依据前置操作数的条件真值逐级收窄。
;;
;; 语法
;; ----
;; (walk-and walk rel expr env locals acc)
;;
;; 参数
;; ----
;; walk : procedure  AST 节点遍历递归函数
;; rel : string      相对文件路径
;; expr : pair       and 表达式 AST
;; env : any         环境
;; locals : list     局部变量类型表
;; acc : list        违规累加器
;;
;; 返回值
;; ------
;; list
;; 累加器结果。

(import (scheme base) (liii check) (liii goldcheck type-hint narrow))

(check-set-mode! 'report-failed)

(let* ((seen '())
       (mock-walk
         (lambda (rel e env locals acc) (set! seen (cons (cons e locals) seen)) acc)
       ) ;mock-walk
       (expr '(and (string? x) (use x)))
       (locals '((x or #f string?)))
      ) ;
  (walk-and mock-walk "a.scm" expr #f locals '())
  ;; 遍历 (use x) 时 x 应当已经被收窄为 string?
  (let ((use-locals (cdr (assoc '(use x) seen))))
    (check (cdr (assq 'x use-locals)) => 'string?)
  ) ;let
) ;let*

(check-report)
