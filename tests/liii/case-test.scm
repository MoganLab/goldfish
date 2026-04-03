;; (liii case) 模块测试文件
;;
;; case* 是一个强大的模式匹配宏，相比标准 case 表达式，它提供了：
;; 1. 数据结构模式匹配（列表、向量）
;; 2. 变量绑定与捕获
;; 3. 省略号匹配（可变长度）
;; 4. 谓词函数匹配
;; 5. 递归嵌套模式
;;
;; 核心用途：在 Scheme 中实现类似 Haskell/OCaml 的模式匹配功能，
;; 特别适用于解析树结构、处理 AST、实现解释器等场景。

;; ==== 语法说明 ====
;;
;; (case* <选择器>
;;   (<模式1> <结果1>)
;;   (<模式2> <结果2>)
;;   ...
;;   (else <默认结果>))
;;
;; <模式> 可以是：
;; - 字面量：数字、字符串、符号等
;; - 列表模式：(1 2 3) 完全匹配列表
;; - 向量模式：#(1 2 3) 完全匹配向量
;; - 标签绑定：#<label:> 捕获匹配值，在结果中用 #<label> 引用
;; - 谓词匹配：#<predicate?> 只匹配满足谓词的值
;; - 省略号：#<...> 或 #<label:...> 匹配任意数量的元素
;;
;; 重要：标签捕获的值在结果中通过 #<label>（不带冒号）来引用

;; ========== 性能与使用注意事项 ==========

;; 注意1：case* 是宏展开，模式在编译时处理
;; 注意2：复杂的嵌套模式可能影响编译时间
;; 注意3：标签绑定使用哈希表存储，查找时间为 O(1)
;; 注意4：省略号匹配会创建新列表，注意内存使用
;; 注意5：在结果中引用标签时，必须使用 #<label> 语法（不带冒号）


;; ==== 函数分类索引 ====
;;
;; 一、模式匹配宏
;;   case*            - 主模式匹配宏，支持各种模式语法

;; ==== 单元测试 ====

(import (liii check)
        (liii case)
) ;import

(check-set-mode! 'report)

;; ========== 基础字面量匹配测试 ==========

;; 基本符号匹配
(check (case* 'yes
         ((yes no) 'boolean)
         (else 'unknown))
  => 'boolean)

(check (case* 'no
         ((yes no) 'boolean)
         (else 'unknown))
  => 'boolean)

(check (case* 'maybe
         ((yes no) 'boolean)
         (else 'unknown))
  => 'unknown)

;; 数字匹配
(check (case* 42
         ((1 2 3) 'small)
         ((42 100) 'big)
         (else 'other))
  => 'big)

;; 字符串匹配
(check (case* "hello"
         (("hi" "hello") 'greeting)
         (else 'other))
  => 'greeting)

;; 混合类型匹配
(check (case* 3.14
         ((1 2 3) 'integer)
         ((3.14 2.71) 'float)
         (else 'other))
  => 'float)

;; ========== 列表字面量模式匹配测试 ==========

;; 注意：case* 的列表模式需要完全匹配字面量
(check (case* '(1 2 3)
         (((1 2 3)) 'matched)
         (else 'not-matched))
  => 'matched)

;; 不匹配的情况
(check (case* '(1 2 3)
         (((1 2)) 'two)
         (((1 2 3)) 'three)
         (else 'other))
  => 'three)

;; ========== 标签绑定测试 ==========

;; 简单标签捕获 - 注意：结果中用 #<x> 而非 x 引用
(check (case* '(1 2)
         (((#<x:> #<y:>)) (+ #<x> #<y>))
         (else 0))
  => 3)

;; 在结果中使用标签 - 标签引用语法 #<label>
(check (case* '(hello world)
         (((#<first:> #<second:>)) (list '#<second> '#<first>))
         (else '()))
  => '(world hello))

;; 标签值比较
(check (case* '(5 5)
         (((#<x:> #<y:>)) (if (= #<x> #<y>) 'same 'different))
         (else 'unknown))
  => 'same)

;; ========== 谓词匹配测试 ==========

;; 基本谓词匹配 - 使用 #<predicate?> 语法
(check (case* 42
         ((#<integer?>) 'integer)
         ((#<string?>) 'string)
         (else 'other))
  => 'integer)

(check (case* "hello"
         ((#<integer?>) 'integer)
         ((#<string?>) 'string)
         (else 'other))
  => 'string)

;; 带标签的谓词匹配 - 使用 #<label:predicate?> 语法，结果中用 #<label>
(check (case* 42
         ((#<x:integer?>) (* #<x> 2))
         (else 0))
  => 84)

;; 自定义谓词函数
(check (case* 10
         ((#<even?>) 'even)
         ((#<odd?>) 'odd)
         (else 'unknown))
  => 'even)

(check (case* 7
         ((#<even?>) 'even)
         ((#<odd?>) 'odd)
         (else 'unknown))
  => 'odd)

;; ========== 向量模式匹配测试 ==========

;; 基本向量匹配
(check (case* #(1 2 3)
         ((#(1 2 3)) 'matched)
         (else 'no))
  => 'matched)

;; 向量中的标签捕获
(check (case* #(10 20 30)
         ((#(#<x:> 20 #<y:>)) (list #<x> #<y>))
         (else '()))
  => '(10 30))

;; ========== else 子句测试 ==========

;; else 作为默认分支
(check (case* 'unknown-value
         ((a b c) 'abc)
         ((x y z) 'xyz)
         (else 'default))
  => 'default)

;; ========== 边界情况测试 ==========

;; 空列表匹配
(check (case* '()
         ((()) 'empty)
         (else 'not-empty))
  => 'empty)

;; 单元素列表
(check (case* '(only)
         ((#<x:>) '#<x>)
         (else 'none))
  => '(only))

;; 单元素向量
(check (case* #(42)
         ((#(42)) 'forty-two)
         (else 'none))
  => 'forty-two)

;; ========== 实际应用示例测试 ==========

;; 1. 符号代数简化
(define (simplify expr)
  (case* expr
    (((+ 0 #<x:>)) '#<x>)                    ; (+ 0 x) => x
    (((+ #<x:> 0)) '#<x>)                    ; (+ x 0) => x
    (((* 1 #<x:>)) '#<x>)                    ; (* 1 x) => x
    (((* #<x:> 1)) '#<x>)                    ; (* x 1) => x
    (((* 0 #<...>)) 0)                       ; (* 0 ...) => 0
    (else expr)))

(check (simplify '(+ 0 x)) => 'x)
(check (simplify '(+ x 0)) => 'x)
(check (simplify '(* 1 y)) => 'y)
(check (simplify '(* y 1)) => 'y)
(check (simplify '(* 0 anything here)) => 0)
(check (simplify '(+ 1 2)) => '(+ 1 2))    ; 不匹配任何模式

;; 2. 表达式类型检查
(define (expr-type expr)
  (case* expr
    (((lambda (#<args:...>) #<body:>)) 'lambda)
    (((if #<cond:> #<then:> #<else:>)) 'conditional)
    (((#<op:symbol?> #<args:...>)) 'application)
    ((#<x:integer?>) 'integer-literal)
    ((#<x:symbol?>) 'variable)
    (else 'unknown)))

(check (expr-type '(lambda (x) x)) => 'lambda)
(check (expr-type '(if a b c)) => 'conditional)
(check (expr-type '(+)) => 'application)
(check (expr-type 42) => 'integer-literal)
(check (expr-type 'variable) => 'variable)
(check (expr-type #(1 2 3)) => 'unknown)

;; 3. 列表解构
(define (list-info lst)
  (case* lst
    ((()) 'empty)
    (((#<x:>)) (list 'single '#<x>))
    (((#<a:> #<b:> #<rest:...>)) (list 'multiple #<a> #<b> #<rest>))
    (else 'other)))

(check (list-info '()) => 'empty)
(check (list-info '(one)) => '(single one))
(check (list-info '(1 2 3 4)) => '(multiple 1 2 (3 4)))
(check (list-info '(1 2)) => '(multiple 1 2 ()))

;; 4. 简单的模式匹配计算器
(define (calc expr)
  (case* expr
    (((+ #<a:integer?> #<b:integer?>)) (+ #<a> #<b>))
    (((- #<a:integer?> #<b:integer?>)) (- #<a> #<b>))
    (((#<op:> #<args:...>)) (list 'unhandled-op '#<op> #<args>))
    ((#<x:integer?>) #<x>)
    (else 'invalid)))

(check (calc '(+ 3 4)) => 7)
(check (calc '(- 10 3)) => 7)
(check (calc '(* 2 3)) => '(unhandled-op * (2 3)))
(check (calc 42) => 42)

;; 5. 数据结构验证
(define (validate-user data)
  (case* data
    (((user (name #<n:string?>) (age #<a:integer?>)))
     (and (> #<a> 0) (< #<a> 150)))
    (else #f)))

(check (validate-user '(user (name "Alice") (age 30))) => #t)
(check (validate-user '(user (name "Bob") (age 200))) => #f)
(check (validate-user '(user (name 123) (age 30))) => #f)
(check (validate-user '(other (name "X") (age 20))) => #f)

;; ========== 复杂模式匹配示例 ==========

;; 匹配二元运算表达式并提取操作符和操作数
(define (binop-expr? expr)
  (case* expr
    (((#<op:symbol?> #<left:> #<right:>))
     (list 'binop '#<op> '#<left> '#<right>))
    (else #f)))

(check (binop-expr? '(+ 1 2)) => '(binop + 1 2))
(check (binop-expr? '(* x y)) => '(binop * x y))
(check (binop-expr? '(+ 1 2 3)) => #f)    ; 三个操作数不匹配
(check (binop-expr? 42) => #f)

;; 匹配 if 表达式
(define (if-expr? expr)
  (case* expr
    (((if #<cond:> #<then:> #<else:>))
     (list 'if-expr '#<cond> '#<then> '#<else>))
    (else #f)))

(check (if-expr? '(if (> x 0) x (- x)))
  => '(if-expr (> x 0) x (- x)))
(check (if-expr? '(if flag then)) => #f)   ; 缺少 else 分支

(check-report)
