(import (liii check) (liii tree))

(check-set-mode! 'report-failed)

;; tree-depth
;; 计算树结构（嵌套列表）的最大嵌套深度。
;;
;; 语法
;; ----
;; (tree-depth tree)
;;
;; 参数
;; ----
;; tree : any
;; 待计算深度的树结构（支持嵌套列表、点对以及原子）。
;;
;; 返回值
;; ------
;; integer
;; 树结构的最大嵌套层数。
;;
;; 说明
;; ----
;; tree-depth 采用树节点模型计算树结构的最大嵌套深度：
;; - 非 pair 元素（包括数字、符号、字符串等原子）以及空列表 '() 视作叶子节点，深度为 0；
;; - 对 (quote x) 形式做透明处理，直接递归计算其内部结构 x 的嵌套深度，避免 'sym 符号字面量被 reader 展开为列表而虚增深度；
;; - 不含嵌套 pair 的列表（如 '(1 2 3) 或 '(1 () 2)）深度为 1；
;; - 包含嵌套 pair 时，深度为 1 + 各子项深度的最大值；
;; - 若树结构中包含环（循环引用），将抛出 value-error 错误。
;;
;; 错误
;; ----
;; value-error
;; 当传入存在循环引用的树结构时抛出。

;; 1. 叶子节点：原子与空列表（深度 0）
(check (tree-depth 1) => 0)
(check (tree-depth 'a) => 0)
(check (tree-depth "string") => 0)
(check (tree-depth '()) => 0)

;; 2. 扁平列表与仅含空列表叶子的列表（深度 1）
(check (tree-depth '(1)) => 1)
(check (tree-depth '(1 2 3)) => 1)
(check (tree-depth '(a b c d)) => 1)
(check (tree-depth '(())) => 1)
(check (tree-depth '(1 () 2)) => 1)

;; 3. 二层嵌套（深度 2）
(check (tree-depth '((()))) => 2)
(check (tree-depth '((1))) => 2)
(check (tree-depth '((1 2) 3)) => 2)
(check (tree-depth '(1 (2 3))) => 2)
(check (tree-depth '((1 2) (3 4))) => 2)
(check (tree-depth '(1 () (2 ()))) => 2)

;; 4. 多层与非对称嵌套（深度 >= 3）
(check (tree-depth '(((())))) => 3)
(check (tree-depth '(((1)))) => 3)
(check (tree-depth '(1 (2 (3)))) => 3)
(check (tree-depth '((1 2) (3 (4 (5))))) => 4)
(check (tree-depth '((a) ((b) (((c)))))) => 5)

;; 5. 点对结构（pair）
(check (tree-depth '(1 . 2)) => 1)
(check (tree-depth '(1 . (2 . 3))) => 1)
(check (tree-depth '((1 . 2) . 3)) => 2)

;; 6. 带 quote 的形式（quote 作为字面量语法糖不额外增加嵌套深度）
(check (tree-depth ''a) => 0)
(check (tree-depth '(quote a)) => 0)
(check (tree-depth '(a 'b)) => 1)
(check (tree-depth '(a 'b 'c)) => 1)
(check (tree-depth '(assoc 'default options)) => 1)
(check (tree-depth '((found (assoc 'default options)))) => 3)
(check (tree-depth '(quote (1 2 3))) => 1)
(check (tree-depth '(quote ((1 2) (3 (4 (5)))))) => 4)

;; 7. 环状结构异常检测
(let ((cyclic-list (list 1 2)))
  (set-cdr! (cdr cyclic-list) cyclic-list)
  (check-catch 'value-error (tree-depth cyclic-list)))

(let* ((inner (list 1))
       (outer (list inner 2)))
  (set-cdr! inner outer)
  (check-catch 'value-error (tree-depth outer)))

(check-report)
