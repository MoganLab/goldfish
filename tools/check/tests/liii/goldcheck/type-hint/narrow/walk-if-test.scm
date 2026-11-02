;; walk-if
;; 遍历 if 控制流表达式，在 then 分支应用测试条件的类型收窄。
;;
;; 语法
;; ----
;; (walk-if walk rel expr env locals acc)
;;
;; 参数
;; ----
;; walk : procedure  AST 节点遍历递归函数
;; rel : string      相对文件路径
;; expr : pair       if 表达式 AST
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
       (expr '(if (string? x) (then-body x) (else-body x)))
       (locals '((x or #f string?)))
      ) ;
  (walk-if mock-walk "a.scm" expr #f locals '())
  ;; then-body 处 x 应当已收窄为 string?
  (let ((then-locals (cdr (assoc '(then-body x) seen))))
    (check (cdr (assq 'x then-locals)) => 'string?)
  ) ;let
  ;; else-body 处 x 应当保持原类型 (or #f string?)
  (let ((else-locals (cdr (assoc '(else-body x) seen))))
    (check (cdr (assq 'x else-locals)) => '(or #f string?))
  ) ;let
) ;let*

(check-report)
