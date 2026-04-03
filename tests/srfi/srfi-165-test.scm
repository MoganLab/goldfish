(import (liii check)
        (srfi srfi-165)
) ;import

(check-set-mode! 'report-failed)

#|
make-computation-environment-variable
创建可变的计算环境变量。

语法
----
(make-computation-environment-variable name default immutable?)

参数
----
name : symbol
    变量的符号名，用于标识和调试
default : any
    变量的默认值，当环境中未绑定此变量时返回该值
immutable? : boolean
    若为 #t，则禁止通过 computation-environment-update! 修改；
    若为 #f，允许动态绑定和修改

返回值
----
computation-environment-variable?
    新创建的环境变量对象

说明
----
环境变量是计算环境（computation environment）中的可配置单元，
用于在计算过程中传递隐式参数。每个变量对象具有唯一标识，
即使 name、default 和 immutable? 相同，eq? 也为 #f。

变量绑定按优先级查找：local 层 > global 层 > 默认值。
|#


(define test-var-1
  (make-computation-environment-variable 'test-var-1 "default" #f)
) ;define
(define test-var-2
  (make-computation-environment-variable 'test-var-2 42 #t)
) ;define
(define test-var-1*
  (make-computation-environment-variable 'test-var-1 "default" #f)
) ;define

; 变量唯一性
(check-false (eq? test-var-1 test-var-1*))


#|
make-computation-environment
创建空的计算环境。

语法
----
(make-computation-environment)

参数
----
无

返回值
----
vector?
    新的计算环境对象（实现相关，通常为向量结构）

说明
----
计算环境是存储变量绑定的容器，支持分层查找机制（local/global）。
新创建的环境包含预定义的系统变量绑定（如 default-computation）。
每次调用创建独立的环境实例。
|#

(define env-1 (make-computation-environment))
(define env-2 (make-computation-environment))

; 基本属性
(check-true (vector? env-1))  ; 实现细节，但可验证创建成功
(check-false (eq? env-1 env-2))  ; 每次创建独立环境

; 新环境包含默认变量绑定（如 default-computation）
(check (computation-environment-ref env-1 default-computation) => #f)


#|
computation-environment-ref
查询环境中变量的当前值。

语法
----
(computation-environment-ref environment variable)

参数
----
environment : computation-environment?
    要查询的计算环境
variable : computation-environment-variable?
    之前通过 make-computation-environment-variable 创建的变量

返回值
----
any
    变量的当前绑定值；若未绑定，返回变量的默认值

说明
----
查找顺序：先检查 local 层（函数式更新创建），再检查 global 层
（破坏性更新创建），最后返回变量定义时的默认值。

若 variable 不是有效的环境变量对象，可能抛出类型错误。
|#

(define var-x (make-computation-environment-variable 'x 100 #f))
(define var-y (make-computation-environment-variable 'y 200 #f))

; 查询默认值
(check (computation-environment-ref env-1 var-x) => 100)
(check (computation-environment-ref env-1 var-y) => 200)

(check-catch 'wrong-type-arg (computation-environment-ref 'not-env var-x))

#|
computation-environment-update
函数式更新环境，创建包含新绑定的新环境。

语法
----
(computation-environment-update environment variable value ...)
(computation-environment-update environment variable value ...)

参数
----
environment : computation-environment?
    基础环境
variable : computation-environment-variable?
    要绑定的变量
value : any
    对应变量的新值
... : 支持多组 variable/value 对

返回值
----
computation-environment?
    包含新绑定的新环境，原环境保持不变

说明
----
创建的绑定位于 local 层，会遮蔽同一变量在 global 层或默认值的绑定。
原环境对象未被修改，符合函数式语义，适合用于 computation-local
或 computation-with。

多变量更新是原子的，所有绑定在同一新环境中生效。
|#

(define env-updated (computation-environment-update env-1 var-x 999))

; 原环境不变
(check (computation-environment-ref env-1 var-x) => 100)

; 新环境有更新值
(check (computation-environment-ref env-updated var-x) => 999)

; 多变量更新
(define env-multi
  (computation-environment-update env-1 var-x 1 var-y 2)
) ;define
(check (computation-environment-ref env-multi var-x) => 1)
(check (computation-environment-ref env-multi var-y) => 2)

; 更新创建 local 层遮蔽 global
(check
 (let ((global-env
        (begin
          (computation-environment-update! env-1 var-x 'global)
          env-1))
        ) ;begin
   (computation-environment-ref
    (computation-environment-update global-env var-x 'local)
    var-x
   ) ;computation-environment-ref
 ) ;let
 => 'local
) ;check


#|
computation-environment-update!
破坏性更新环境，修改现有环境或创建 global 绑定。

语法
----
(computation-environment-update! environment variable value)

参数
----
environment : computation-environment?
    要修改的环境
variable : computation-environment-variable?
    要绑定的变量
value : any
    新值

返回值
----
未指定

说明
----
若 variable 在当前环境已有绑定（任何层），则修改该绑定；
否则在 global 层创建新绑定。此操作修改传入的 environment 对象，
影响所有引用该环境的计算。

对标记为 immutable? 的变量调用此过程暂无报错。见 computation-environment-update! 实现中。
|#

(define env-mutable (make-computation-environment))

; 更新预定义变量
(computation-environment-update! env-mutable var-x 'modified)
(check (computation-environment-ref env-mutable var-x) => 'modified)

; 更新动态变量（创建 global 绑定）
(define dynamic-var
  (make-computation-environment-variable 'dynamic 'initial #f)
) ;define
(computation-environment-update! env-mutable dynamic-var 'new-value)
(check (computation-environment-ref env-mutable dynamic-var) => 'new-value)

; 多次更新覆盖
(computation-environment-update! env-mutable var-x 'first)
(computation-environment-update! env-mutable var-x 'second)
(check (computation-environment-ref env-mutable var-x) => 'second)



#|
make-computation
从显式 continuation 过程创建计算对象。

语法
----
(make-computation proc)

参数
----
proc : procedure
    接受一个参数（compute 函数）的过程
    (lambda (compute) ...)

返回值
----
computation?
    延迟的计算对象

说明
----
proc 在被执行时接收 compute 函数，通过调用 (compute comp) 来
继续执行指定计算。允许在 continuation 中执行副作用后再继续。

这是构造计算的基本底层构造器，通常与 computation-bind、
computation-pure 等组合使用。
|#

; 手动调用 continuation
(check
 (computation-run
   (make-computation
     (lambda (compute)
       (compute (computation-pure 42))
     ) ;lambda
   ) ;make-computation
 ) ;computation-run
 => 42
) ;check

; continuation 可多次调用
(let ((c (make-computation
           (lambda (compute)
             (compute (computation-pure 'first)))))
           ) ;lambda
  (check (computation-run c) => 'first)
  (check (computation-run c) => 'first)
) ;let

; 与 ask 结合使用环境
(check
 (let ((test-var (make-computation-environment-variable 'test 100 #f)))
   (computation-run
     (computation-with ((test-var 999))
       (make-computation
         (lambda (compute)
           (compute
             (computation-fn ((v test-var))
               (computation-pure v)
             ) ;computation-fn
           ) ;compute
         ) ;lambda
       ) ;make-computation
     ) ;computation-with
   ) ;computation-run
 ) ;let
 => 999
) ;check

; 嵌套 make-computation
(check
 (computation-run
   (make-computation
     (lambda (compute)
       (compute
         (make-computation
           (lambda (compute2)
             (compute2 (computation-pure 'nested))
           ) ;lambda
         ) ;make-computation
       ) ;compute
     ) ;lambda
   ) ;make-computation
 ) ;computation-run
 => 'nested
) ;check

; 与 bind 的交互（左侧）
(check
 (computation-run
   (computation-bind
     (make-computation
       (lambda (compute)
         (compute (computation-pure 10))
       ) ;lambda
     ) ;make-computation
     (lambda (x)
       (computation-pure (* x 2))
     ) ;lambda
   ) ;computation-bind
 ) ;computation-run
 => 20
) ;check



#|
computation-run
在全新环境中执行计算并返回结果。

语法
----
(computation-run computation)

参数
----
computation : computation?
    要执行的计算对象

返回值
----
any
    计算产生的值；多值计算返回多个值

说明
----
每次调用创建全新的计算环境（包含默认变量绑定），确保计算
执行的隔离性和可重现性。环境在计算开始前初始化，计算结束后
丢弃。

这是脱离计算组合子（monadic context）获取实际结果的入口点。
|#

; 基本执行
(check (computation-run (computation-pure 'test)) => 'test)

; 环境隔离（每次 run 创建新环境）
(check
 (let ((counter
        (make-computation-environment-variable 'counter 0 #f)))
   (computation-run
     (computation-bind
       (computation-with! (counter 1))
       (lambda (_)
         (computation-fn ((c counter)) (computation-pure c))
       ) ;lambda
     ) ;computation-bind
   ) ;computation-run
 ) ;let
 => 1
) ;check

(check
 (let ((counter
        (make-computation-environment-variable 'counter 0 #f)))
   (computation-run
     (computation-each
       (computation-with! (counter 2))
       (computation-fn ((c counter)) (computation-pure c))
     ) ;computation-each
   ) ;computation-run
 ) ;let
 => 2
) ;check

(check
 (computation-run
   (computation-fn ((c (make-computation-environment-variable 'counter 0 #f)))
     (computation-pure c)
   ) ;computation-fn
 ) ;computation-run
 => 0  ; 新环境重置为默认值
) ;check


#|
computation-ask
获取当前计算环境的计算。

语法
----
(computation-ask)

返回值
----
computation?
    产生当前环境的计算对象

说明
----
常用于在计算过程中访问隐式环境参数。通常与 computation-bind
组合使用，将环境传递给需要显式操作环境的过程。

等价于 (computation-pure <current-environment>)，但延迟获取
直到运行时。
|#

(define var-ask (make-computation-environment-variable 'ask 42 #f))

; 与 bind 结合使用
(check
 (computation-run
   (computation-bind (computation-ask)
     (lambda (env)
       (computation-pure (computation-environment-ref env var-ask))
     ) ;lambda
   ) ;computation-bind
 ) ;computation-run
 => 42  ; 使用 env-ask 的默认值，或根据环境而定（这里没有修改过）
) ;check


#|
computation-local
在修改后的环境中执行子计算。

语法
----
(computation-local updater computation)

参数
----
updater : procedure
    接受当前环境并返回新环境的函数
    (lambda (env) -> new-env)
computation : computation?
    要在新环境中执行的计算

返回值
----
computation?
    延迟的计算对象，执行时环境已被修改

说明
----
updater 通常使用 computation-environment-update 创建局部绑定。
子计算执行期间，修改后的环境生效；子计算结束后，原环境恢复。
这是函数式修改环境的推荐方式。
|#

(define var-local (make-computation-environment-variable 'local 'global-val #f))

(check
 (computation-run
   (computation-local
     (lambda (env) (computation-environment-update env var-x 'local-val))
     (computation-bind (computation-ask)
       (lambda (e)
         (computation-pure (computation-environment-ref e var-x))
       ) ;lambda
     ) ;computation-bind
   ) ;computation-local
 ) ;computation-run
 => 'local-val
) ;check

; 原环境不受影响
(check (computation-environment-ref env-1 var-local) => 'global-val)

#|
computation-pure
将值提升为纯计算（monadic return）。

语法
----
(computation-pure value ...)
(computation-pure value ...)

参数
----
value : any
    要包装的值
... : 支持多值

返回值
----
computation?
    产生指定值的延迟计算

说明
----
创建立即成功的计算，不依赖环境，不执行副作用。是多值返回、
纯值注入计算上下文的标准方式。

与 computation-bind 结合构成 Monad 的基本操作。
|#

(check (computation-run (computation-pure 42)) => 42)
(check (computation-run (computation-pure 'hello)) => 'hello)

; 多值
(check
 (call-with-values
   (lambda () (computation-run (computation-pure 1 2 3)))
   list
 ) ;call-with-values
 => '(1 2 3)
) ;check


#|
computation-each
顺序执行多个计算，返回最后一个的结果。

语法
----
(computation-each computation ...)
(computation-each computation ...)

参数
----
computation : computation?
    要顺序执行的计算
... : 一个或多个计算

返回值
----
any
    最后一个计算的结果

说明
----
按从左到右顺序执行计算，忽略前面计算的结果（仅利用其副作用），
最终返回最后一个计算产生的值。用于强制副作用执行顺序。

若任一计算失败或抛出异常，后续计算不执行。
|#

; each 多个参数
(check
 (computation-run
   (computation-each (computation-pure 1)
                     (computation-pure 2)
                     (computation-pure 3)
   ) ;computation-each
 ) ;computation-run
 => 3
) ;check

; 副作用顺序验证
(check
 (let ((result '()))
   (computation-run
     (computation-each
       (make-computation (lambda (k) (set! result (cons 1 result)) (k (computation-pure 'void))))
       (make-computation (lambda (k) (set! result (cons 2 result)) (k (computation-pure 'void))))
       (make-computation (lambda (k) (set! result (cons 3 result)) (k (computation-pure 'void))))
     ) ;computation-each
   ) ;computation-run
   result
 ) ;let
 => '(3 2 1)
) ;check


#|
computation-each-in-list
对列表中的计算顺序执行，返回最后一个的结果。

语法
----
(computation-each-in-list list)

参数
----
list : list
    计算对象（computation?）的列表

返回值
----
any
    最后一个计算的结果；空列表返回未指定值

说明
----
computation-each 的列表版本。空列表行为与 each 相同（返回
(computation-pure <unspecified>)）。
|#

; each-in-list 列表形式
(check
 (computation-run
   (computation-each-in-list
     (list (computation-pure 'a)
           (computation-pure 'b)
           (computation-pure 'c)
     ) ;list
   ) ;computation-each-in-list
 ) ;computation-run
 => 'c
) ;check

#|
computation-bind
顺序组合计算（monadic bind）。

语法
----
(computation-bind computation proc)

参数
----
computation : computation?
    前置计算
proc : procedure
    接受前置计算结果，返回新计算的函数
    (lambda (result ...) -> computation?)

返回值
----
computation?
    组合后的延迟计算

说明
----
执行前置计算，将其结果（支持多值）传递给 proc，执行 proc
返回的新计算。实现计算的顺序依赖和结果传递。

是构造复杂计算流程的核心组合子。
|#

; 基本绑定
(check
 (computation-run
   (computation-bind (computation-pure 5)
     (lambda (x) (computation-pure (* x 2)))
   ) ;computation-bind
 ) ;computation-run
 => 10
) ;check

; 多值传递
(check
 (computation-run
   (computation-bind (computation-pure 1 2)
     (lambda (a b) (computation-pure (+ a b)))
   ) ;computation-bind
 ) ;computation-run
 => 3
) ;check

; 链式绑定
(check
 (computation-run
   (computation-bind (computation-pure 10)
     (lambda (x)
       (computation-bind (computation-pure 20)
         (lambda (y)
           (computation-pure (+ x y))
         ) ;lambda
       ) ;computation-bind
     ) ;lambda
   ) ;computation-bind
 ) ;computation-run
 => 30
) ;check


#|
computation-sequence
将计算列表转换为产生结果列表的计算。

语法
----
(computation-sequence list)

参数
----
list : list
    计算对象（computation?）的列表

返回值
----
computation?
    执行后产生结果列表的计算

说明
----
保持列表顺序执行计算，收集所有结果组成新列表。空列表返回
(computation-pure '())。

副作用按从左到右顺序发生。
|#

; 基础序列
(check
 (computation-run
   (computation-sequence
     (list (computation-pure 1)
           (computation-pure 2)
           (computation-pure 3)
     ) ;list
   ) ;computation-sequence
 ) ;computation-run
 => '(1 2 3)
) ;check

; 空列表
(check (computation-run (computation-sequence '())) => '())

; 保持顺序
(check
 (computation-run
   (computation-sequence
     (list (computation-pure 'a)
           (computation-pure 'b)
     ) ;list
   ) ;computation-sequence
 ) ;computation-run
 => '(a b)
) ;check

; 副作用顺序（从左到右）
(check
 (let ((n 0))
   (computation-run
     (computation-sequence
       (list
         (make-computation (lambda (k) (set! n (+ n 1)) (k (computation-pure n))))
         (make-computation (lambda (k) (set! n (+ n 1)) (k (computation-pure n))))
         (make-computation (lambda (k) (set! n (+ n 1)) (k (computation-pure n))))
       ) ;list
     ) ;computation-sequence
   ) ;computation-run
   n
 ) ;let
 => 3
) ;check

#|
computation-forked
在环境副本中并行执行计算，原环境保持不变。

语法
----
(computation-forked computation ...)
(computation-forked computation ...)

参数
----
computation : computation?
    要执行的计算
... : 一个或多个计算

返回值
----
computation?
    产生最后一个计算结果的延迟计算

说明
----
每个计算在环境的独立浅拷贝中执行，彼此隔离。
任一计算对环境的修改不影响其他计算和原环境。
适用于需要临时修改环境但不希望影响后续计算的场景。
|#

(check
 (computation-run
   (computation-with ((var-x 'shared))
     (computation-forked
       (computation-with! (var-x 'branch1))
       (computation-fn ((x var-x)) (computation-pure x))  ; 返回 'shared，不受 branch1 影响
     ) ;computation-forked
   ) ;computation-with
 ) ;computation-run
 => 'shared
) ;check

; 多分支执行
(check
 (computation-run
   (computation-with ((var-x 0))
     (computation-forked
       (computation-with! (var-x 999))  ; 在副本执行
       (computation-pure 'done)        ; 在原始环境执行，不改环境
     ) ;computation-forked
     (computation-fn ((x var-x))
       (computation-pure x)
     ) ;computation-fn
   ) ;computation-with
 ) ;computation-run
 => 0
) ;check


#|
computation-bind/forked
先复制环境再执行绑定的计算组合。

语法
----
(computation-bind/forked computation proc)

参数
----
computation : computation?
    前置计算（在复制后的环境执行）
proc : procedure
    接受结果，返回后续计算的函数

返回值
----
computation?
    组合计算，proc 返回的计算在原始环境执行

说明
----
前置计算在环境副本中执行，proc 及其返回的计算在原始环境执行。
实现“隔离执行前置计算，但后续计算继承原环境”的模式。
|#

(check
 (computation-run
   (computation-with ((var-x 'original))
     (computation-bind/forked
       (computation-with! (var-x 'changed))
       (lambda (_)
         (computation-fn ((x var-x))
           (computation-pure x)
         ) ;computation-fn
       ) ;lambda
     ) ;computation-bind/forked
   ) ;computation-with
 ) ;computation-run
 => 'original
) ;check


#|
computation-fn
从环境提取变量绑定到本地标识符。

语法
----
(computation-fn (binding ...) computation)

binding : identifier
       | (identifier variable)

参数
----
binding :
    identifier - 简写形式，变量名与绑定名相同
    (identifier variable) - 显式指定变量和本地名
variable : computation-environment-variable?
    要提取的环境变量
computation : computation?
    使用绑定执行的主体计算

返回值
----
computation?
    延迟计算，执行时标识符在作用域内

说明
----
语法糖，等价于使用 computation-bind 和 computation-ask 获取
环境后多次调用 computation-environment-ref。

支持简写：(computation-fn (x) ...) 等价于 (computation-fn ((x x)) ...)。
绑定按顺序处理，后续绑定可引用前述绑定的结果。
|#

; 基础绑定
(check
 (computation-run
   (computation-fn ((x var-x))
     (computation-pure x)
   ) ;computation-fn
 ) ;computation-run
 => 100
) ;check

; 简写形式（变量名与绑定名相同）
(check
 (computation-run
   (computation-fn (var-x)
     (computation-pure var-x)
   ) ;computation-fn
 ) ;computation-run
 => 100
) ;check

; 多变量与中间表达式
(check
 (computation-run
   (computation-fn ((x var-x) (y var-y))
     (let ((sum (+ x y)))
       (computation-pure sum)
     ) ;let
   ) ;computation-fn
 ) ;computation-run
 => 300
) ;check


#|
computation-with
局部绑定变量（函数式），创建新环境执行计算。

语法
----
(computation-with ((variable value) ...) computation ...)

参数
----
variable : computation-environment-variable?
    要绑定的环境变量
value : any
    绑定值
computation : computation?
    在新环境中执行的计算
... : 多个计算按 each 语义顺序执行

返回值
----
computation?
    延迟计算，执行时变量已绑定

说明
----
使用 computation-environment-update 创建局部绑定，不影响外部
环境。支持嵌套，内层绑定遮蔽外层同名变量。

多计算按 computation-each 语义执行，返回最后一个结果。
|#

; 单变量
(check
 (computation-run
   (computation-with ((var-x 999))
     (computation-fn ((x var-x)) (computation-pure x))
   ) ;computation-with
 ) ;computation-run
 => 999
) ;check

; 多变量与多计算
(check
 (computation-run
   (computation-with ((var-x 1) (var-y 2))
     (computation-each (computation-pure 'ignored))
     (computation-fn ((x var-x) (y var-y))
       (computation-pure (list x y))
     ) ;computation-fn
   ) ;computation-with
 ) ;computation-run
 => '(1 2)
) ;check

; 嵌套遮蔽
(check
 (computation-run
   (computation-with ((var-x 'outer))
     (computation-with ((var-x 'inner))
       (computation-fn ((x var-x)) (computation-pure x))
     ) ;computation-with
   ) ;computation-with
 ) ;computation-run
 => 'inner
) ;check


#|
computation-with!
局部修改变量（破坏性），在当前环境直接修改。

语法
----
(computation-with! ((variable value) ...) computation ...)

参数
----
variable : computation-environment-variable?
    要修改的环境变量
value : any
    新值
computation : computation?
    在修改后环境中执行的计算
... : 多个计算

返回值
----
computation?
    延迟计算

说明
----
使用 computation-environment-update! 直接修改当前环境，修改
在 computation 执行期间及之后保持有效（除非被覆盖）。

适用于需要真正改变环境状态的场景，如设置输出端口、修改
计数器等。
|#

(check
 (computation-run
   (computation-each
     (computation-with! (var-x 'temp))
     (computation-fn ((x var-x)) (computation-pure x))
   ) ;computation-each
 ) ;computation-run
 => 'temp
) ;check

; 验证是破坏性修改（通过 ask 查看环境）
(check
 (computation-run
   (computation-each
     (computation-with! (var-x 'modified))
     (computation-fn ((x var-x)) (computation-pure x))
   ) ;computation-each
 ) ;computation-run
 => 'modified
) ;check


#|
define-computation-type
定义新的计算类型及其运行环境。

语法
----
(define-computation-type constructor runner (binding ...))

binding : (variable-name default-value)
        | (variable-name default-value immutable?)

参数
----
constructor : identifier
    创建该类型计算对象的构造函数名
runner : identifier
    执行该类型计算的函数名
binding :
    变量规范列表，定义该计算类型的默认环境变量

返回值
----
未指定（定义宏）

说明
----
创建封闭的计算类型，具有独立的环境变量命名空间和默认绑定。
constructor 等价于 make-computation，runner 等价于 computation-run，
但预装指定的默认变量。

用于封装特定领域（如格式化输出、状态管理）的计算上下文，
避免变量名冲突和重复配置。
|#


(define-computation-type make-show-env show-run
  (port (current-output-port))
  (col 0)
  (row 0)
  (width 78)
  (radix 10)
  (pad-char #\space)
  (substring/width substring)
  (substring/preserve #f)
  (word-separator? char-whitespace?)
  (ambiguous-is-wide? #f)
  (ellipsis "")
  (decimal-align #f)
  (decimal-sep #f)
  (comma-sep #f)
  (comma-rule #f)
  (sign-rule #f)
  (precision #f)
  (writer #f)
) ;define-computation-type

(show-run
  (computation-fn ((p port) (w width))
    (check-true (port? p))
    (check w => 78)
    (computation-pure 'done)
  ) ;computation-fn
) ;show-run

(show-run
  (computation-with ((width 40) (col 10))
     (computation-fn ((w width))
       (check w => 40)
      (computation-pure w)
     ) ;computation-fn
  ) ;computation-with
) ;show-run
(check (show-run (computation-fn (width)
                   (computation-pure width)))
       => 78
) ;check

(show-run
  (computation-each
    (computation-fn ((c col))
      (check c => 0)
      (computation-pure (+ c 1))
    ) ;computation-fn
    (computation-fn ((c col))
      (check c => 0) ; 仍是原值，因环境未变
      (computation-pure 'done)
    ) ;computation-fn
  ) ;computation-each
) ;show-run


(check-report)
