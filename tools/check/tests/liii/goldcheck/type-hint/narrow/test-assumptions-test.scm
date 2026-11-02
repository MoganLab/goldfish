;; test-assumptions
;; 从条件测试表达式提取收窄假设列表 ((变量名 . 假设) ...)。
;;
;; 语法
;; ----
;; (test-assumptions test)
;;
;; 参数
;; ----
;; test : any  条件测试表达式 AST
;;
;; 返回值
;; ------
;; list of pair
;; 收窄假设列表；无法提取假设时返回空列表 '()。

(import (scheme base) (liii check) (liii goldcheck type-hint narrow))

(check-set-mode! 'report-failed)

;; 1. 单个符号为真
(check (test-assumptions 'x) => '((x . truthy)))

;; 2. 类型断言
(check (test-assumptions '(string? s)) => '((s . string?)))

;; 3. and 组合假设
(check (test-assumptions '(and x (integer? y)))
  =>
  '((x . truthy) (y . integer?))
) ;check

;; 4. or / not 不产生确定的收窄假设
(check (test-assumptions '(or a b)) => '())
(check (test-assumptions '(not c)) => '())

(check-report)
