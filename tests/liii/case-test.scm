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


(import (liii check) (liii case))


(check-set-mode! 'report)


;; ========== 基础字面量匹配测试 ==========


;; 基本符号匹配
(check (case* 'yes
        ((yes no) 'boolean)
        (else 'unknown)
       ) ;case*
  =>
  'boolean
) ;check


(check (case* 'no
        ((yes no) 'boolean)
        (else 'unknown)
       ) ;case*
  =>
  'boolean
) ;check


(check (case* 'maybe
        ((yes no) 'boolean)
        (else 'unknown)
       ) ;case*
  =>
  'unknown
) ;check


;; 数字匹配
(check (case* 42
        ((1 2 3) 'small)
        ((42 100) 'big)
        (else 'other)
       ) ;case*
  =>
  'big
) ;check


;; 字符串匹配
(check (case* "hello"
        (("hi" "hello") 'greeting)
        (else 'other)
       ) ;case*
  =>
  'greeting
) ;check


;; 混合类型匹配
(check (case* 3.14
        ((1 2 3) 'integer)
        ((3.14 2.71) 'float)
        (else 'other)
       ) ;case*
  =>
  'float
) ;check


;; ========== 列表字面量模式匹配测试 ==========


;; 注意：case* 的列表模式需要完全匹配字面量
(check (case* '(1 2 3)
        (((1 2 3)) 'matched)
        (else 'not-matched)
       ) ;case*
  =>
  'matched
) ;check


;; 不匹配的情况
(check (case* '(1 2 3)
        (((1 2)) 'two)
        (((1 2 3)) 'three)
        (else 'other)
       ) ;case*
  =>
  'three
) ;check


;; ========== 标签绑定测试 ==========


;; 简单标签捕获 - 注意：结果中用 #<x> 而非 x 引用
(check (case* '(1 2)
        (((#<x:> #<y:>)) (+ #<x> #<y>))
        (else 0)
       ) ;case*
  =>
  3
) ;check


;; 在结果中使用标签 - 标签引用语法 #<label>
;; 注意：'#<label> 中的引号 ' 是 quote 的简写，表示返回符号本身
;; 这里 '#<second> 和 '#<first> 分别被替换为捕获的值 'world 和 'hello
(check (case* '(hello world)
        (((#<first:> #<second:>))
         (list '#<second> '#<first>)
        ) ;
        (else '())
       ) ;case*
  =>
  '(world hello)
) ;check


;; 标签值比较
(check (case* '(5 5)
        (((#<x:> #<y:>))
         (if (= #<x> #<y>) 'same 'different)
        ) ;
        (else 'unknown)
       ) ;case*
  =>
  'same
) ;check


;; ========== 谓词匹配测试 ==========


;; 基本谓词匹配 - 使用 #<predicate?> 语法
(check (case* 42
        ((#<integer?>) 'integer)
        ((#<string?>) 'string)
        (else 'other)
       ) ;case*
  =>
  'integer
) ;check


(check (case* "hello"
        ((#<integer?>) 'integer)
        ((#<string?>) 'string)
        (else 'other)
       ) ;case*
  =>
  'string
) ;check


;; 带标签的谓词匹配 - 使用 #<label:predicate?> 语法，结果中用 #<label>
(check (case* 42
        ((#<x:integer?>) (* #<x> 2))
        (else 0)
       ) ;case*
  =>
  84
) ;check


;; 自定义谓词函数
(check (case* 10
        ((#<even?>) 'even)
        ((#<odd?>) 'odd)
        (else 'unknown)
       ) ;case*
  =>
  'even
) ;check


(check (case* 7
        ((#<even?>) 'even)
        ((#<odd?>) 'odd)
        (else 'unknown)
       ) ;case*
  =>
  'odd
) ;check


;; ========== 向量模式匹配测试 ==========


;; 基本向量匹配
(check (case* #(1 2 3)
        ((#(1 2 3)) 'matched)
        (else 'no)
       ) ;case*
  =>
  'matched
) ;check


;; 向量中的标签捕获
(check (case* #(10 20 30)
        ((#(#<x:> 20 #<y:>)) (list #<x> #<y>))
        (else '())
       ) ;case*
  =>
  '(10 30)
) ;check


;; ========== else 子句测试 ==========


;; else 作为默认分支
(check (case* 'unknown-value
        ((a b c) 'abc)
        ((x y z) 'xyz)
        (else 'default)
       ) ;case*
  =>
  'default
) ;check


;; ========== 边界情况测试 ==========


;; 空列表匹配
(check (case* '()
        ((()) 'empty)
        (else 'not-empty)
       ) ;case*
  =>
  'empty
) ;check


;; 单元素列表
(check (case* '(only)
        ((#<x:>) '#<x>)
        (else 'none)
       ) ;case*
  =>
  '(only)
) ;check


;; 单元素向量
(check (case* #(42)
        ((#(42)) 'forty-two)
        (else 'none)
       ) ;case*
  =>
  'forty-two
) ;check


;; ========== 实际应用示例测试 ==========


;; 1. 符号代数简化
;;
;; 实现要点：
;; - 利用数学恒等式进行表达式简化
;; - 加法恒等式：(+ 0 x) = x, (+ x 0) = x
;; - 乘法恒等式：(* 1 x) = x, (* x 1) = x
;; - 零乘法：(* 0 ...) = 0（任何数乘以0都为0）
;;
;; 标签语法说明：
;; - #<x:>  定义时带冒号，表示"捕获值并绑定到标签x"
;; - #<x>   引用时不带冒号，表示"使用之前捕获的值"
;; - '#<x>  引号'是quote简写，返回捕获值本身（符号或数字）
;;
;; 省略号 #<...> 匹配任意数量的元素，这里用于匹配 (* 0 ...) 的任意参数
;; else 子句在无法简化时原样返回表达式
(define (simplify expr)
  (case* expr
   (((+ 0 #<x:>)) '#<x>)
   (((+ #<x:> 0)) '#<x>)
   (((* 1 #<x:>)) '#<x>)
   (((* #<x:> 1)) '#<x>)
   (((* 0 #<...>)) 0)
   (else expr)
  ) ;case*
) ;define


(check (simplify '(+ 0 x)) => 'x)
(check (simplify '(+ x 0)) => 'x)
(check (simplify '(* 1 y)) => 'y)
(check (simplify '(* y 1)) => 'y)
(check (simplify '(* 0 anything here))
  =>
  0
) ;check
(check (simplify '(+ 1 2)) => '(+ 1 2))


;; 2. 表达式类型检查
;;
;; 实现要点：
;; - 基于 case* 实现简单的 AST（抽象语法树）类型分析器
;; - 模式按优先级从上到下匹配，一旦匹配成功立即返回结果
;; - 顺序很重要：例如 '(+) 必须匹配 application 而非 variable
;;
;; 模式详解（按匹配优先级）：
;; 1. (lambda (#<args:...>) #<body:>)  - Lambda 表达式：字面量lambda + 参数列表 + 函数体
;; 2. (if #<cond:> #<then:> #<else:>)    - 条件表达式：字面量if + 条件 + then分支 + else分支
;; 3. (#<op:symbol?> #<args:...>)      - 函数调用：首元素是符号（谓词匹配），后跟任意参数
;; 4. #<x:integer?>                     - 整数常量：直接用 #<label:predicate?> 谓词匹配
;; 5. #<x:symbol?>                      - 符号变量：同上，用 symbol? 谓词匹配
;;
;; 特殊语法说明：
;; - #<args:...>  带标签的省略号，捕获匹配值到 args 标签
;; - #<op:symbol?> 带标签的谓词匹配，匹配 symbol? 为真的值并绑定到 op
;; - else 子句     当以上都不匹配时返回 'unknown
;;
;; 应用场景：编译器/解释器前端、代码高亮工具、静态分析工具
(define (expr-type expr)
  (case* expr
   (((lambda (#<args:...>) #<body:>))
    'lambda
   ) ;
   (((if #<cond:> #<then:> #<else:>))
    'conditional
   ) ;
   (((#<op:symbol?> #<args:...>))
    'application
   ) ;
   ((#<x:integer?>) 'integer-literal)
   ((#<x:symbol?>) 'variable)
   (else 'unknown)
  ) ;case*
) ;define


(check (expr-type '(lambda (x) x))
  =>
  'lambda
) ;check
(check (expr-type '(if a b c))
  =>
  'conditional
) ;check
(check (expr-type '(+)) => 'application)
(check (expr-type 42)
  =>
  'integer-literal
) ;check
(check (expr-type 'variable)
  =>
  'variable
) ;check
(check (expr-type #(1 2 3)) => 'unknown)


;; 3. 列表解构
;;
;; 实现要点：
;; - 展示如何根据列表长度进行不同的模式匹配
;; - 使用带标签的捕获来提取列表元素
;; - 演示带标签的省略号 #<rest:...> 的用法
;;
;; 模式详解（按匹配顺序）：
;; 1. (())              - 匹配空列表，返回 'empty
;; 2. ((#<x:>))         - 匹配单元素列表，用 #<x> 引用该元素，返回 (single x)
;; 3. (#<a:> #<b:> #<rest:...>) - 匹配两个或更多元素：
;;                                - #<a:> 捕获第一个元素
;;                                - #<b:> 捕获第二个元素
;;                                - #<rest:...> 捕获剩余元素为列表
;; 4. else              - 其他情况返回 'other
;;
;; 引号说明：
;; - '#<x> 返回捕获值本身（符号形式）
;; - #<a> #<b> #<rest> 直接返回值（数字或列表）
;;
;; 应用场景：列表处理、数据结构遍历、模式匹配提取
(define (list-info lst)
  (case* lst
   ((()) 'empty)
   (((#<x:>)) (list 'single '#<x>))
   (((#<a:> #<b:> #<rest:...>))
    (list 'multiple #<a> #<b> #<rest>)
   ) ;
   (else 'other)
  ) ;case*
) ;define


(check (list-info '()) => 'empty)
(check (list-info '(one))
  =>
  '(single one)
) ;check
(check (list-info '(1 2 3 4))
  =>
  '(multiple 1 2 (3 4))
) ;check
(check (list-info '(1 2))
  =>
  '(multiple 1 2 ())
) ;check


;; 4. 简单的模式匹配计算器
(define (calc expr)
  (case* expr
   (((+ #<a:integer?> #<b:integer?>))
    (+ #<a> #<b>)
   ) ;
   (((- #<a:integer?> #<b:integer?>))
    (- #<a> #<b>)
   ) ;
   (((#<op:> #<args:...>))
    (list 'unhandled-op '#<op> #<args>)
   ) ;
   ((#<x:integer?>) #<x>)
   (else 'invalid)
  ) ;case*
) ;define


(check (calc '(+ 3 4)) => 7)
(check (calc '(- 10 3)) => 7)
(check (calc '(* 2 3))
  =>
  '(unhandled-op * (2 3))
) ;check
(check (calc 42) => 42)


;; 5. 数据结构验证
(define (validate-user data)
  (case* data
   (((user (name #<n:string?>)
       (age #<a:integer?>)
     ) ;user
    ) ;
    (and (> #<a> 0) (< #<a> 150))
   ) ;
   (else #f)
  ) ;case*
) ;define


(check (validate-user '(user (name "Alice") (age 30))
       ) ;validate-user
  =>
  #t
) ;check
(check (validate-user '(user (name "Bob") (age 200))
       ) ;validate-user
  =>
  #f
) ;check
(check (validate-user '(user (name 123) (age 30))
       ) ;validate-user
  =>
  #f
) ;check
(check (validate-user '(other (name "X") (age 20))
       ) ;validate-user
  =>
  #f
) ;check


;; ========== 复杂模式匹配示例 ==========


;; 匹配二元运算表达式并提取操作符和操作数
(define (binop-expr? expr)
  (case* expr
   (((#<op:symbol?> #<left:> #<right:>))
    (list 'binop '#<op> '#<left> '#<right>)
   ) ;
   (else #f)
  ) ;case*
) ;define


(check (binop-expr? '(+ 1 2))
  =>
  '(binop + 1 2)
) ;check
(check (binop-expr? '(* x y))
  =>
  '(binop * x y)
) ;check
(check (binop-expr? '(+ 1 2 3)) => #f)
(check (binop-expr? 42) => #f)


;; 匹配 if 表达式
(define (if-expr? expr)
  (case* expr
   (((if #<cond:> #<then:> #<else:>))
    (list 'if-expr
      '#<cond>
      '#<then>
      '#<else>
    ) ;list
   ) ;
   (else #f)
  ) ;case*
) ;define


(check (if-expr? '(if (> x 0) x (- x)))
  =>
  '(if-expr (> x 0) x (- x))
) ;check
(check (if-expr? '(if flag then)) => #f)


(check-report)
