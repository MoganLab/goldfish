(import (liii check))
(import (liii base))
(import (liii json))


(check-set-mode! 'report-failed)


;; openlet
;; 标记一个 let 为 "open"，使内建函数会查询该 let 是否覆盖了某个方法。
;; 返回该 let 自身。
;;
;; 语法
;; ----
;; (openlet let)
;;
;; 参数
;; ----
;; let : let?
;; 要标记为 open 的 let。
;;
;; 返回值
;; ------
;; let?
;; 返回传入的 let（已被标记为 open）。
;;
;; 说明
;; ----
;; openlet 用于对象系统：当一个 let 被 open 后，
;; 内建函数（如 +、display 等）在作用于该 let 的内容时，
;; 会先查询该 let 是否定义了同名的方法，若有则调用之。
;; coverlet 可撤销 openlet 的效果。
;; openlet 就地修改并返回原 let。
;;
;; 示例
;; ----
;; ;; 让 length 内建函数在遇到 e 时调用 e 自己的 'length 方法
;; (length (openlet (inlet 'length (lambda (x) 99))))
;; ;; => 99
;;
;; ;; 让 object->string 查询 e 内的同名方法，自定义字符串表示
;; (object->string (openlet (inlet 'object->string (lambda args "#<myobj>"))))
;; ;; => "#<myobj>"
;;
;; 注意
;; ----
;; 仅对"忽略参数类型"的内建函数（如 length、object->string、display、
;; copy 等）才会触发方法分派；带严格类型检查的函数（如 abs 要求参数
;; 为 number）不会查询 openlet 的方法。


;; openlet 返回传入的 let（eq? 为 #t）
(check (let ((e (inlet 'a 1))) (eq? (openlet e) e)) => #t)


;; openlet 标记后，openlet? 返回 #t
(check (let ((e (inlet 'a 1))) (openlet e) (openlet? e)) => #t)


;; 未经 openlet 的 inlet，openlet? 为 #f
(check (openlet? (inlet 'a 1)) => #f)


;; openlet 后内部绑定仍可访问
(check (let-ref (openlet (inlet 'a 42)) 'a) => 42)


;; 关键用法：openlet 让内建 length 查询到 let 内的同名方法并调用
(check (length (openlet (inlet 'length (lambda (x) 99)))) => 99)


;; openlet 让 object->string 查询同名方法，自定义字符串表示
(check (object->string (openlet (inlet 'object->string (lambda args "#<myobj>"))))
  =>
  "#<myobj>"
) ;check


;; coverlet 可撤销 openlet 的方法分派效果：
;; 撤销后 length 不再调用自定义方法，而返回 inlet 的实际绑定数量（1 个：'length）
(check (length (coverlet (openlet (inlet 'length (lambda (x) 99))))) => 1)


;; 应用示例：用 openlet 定义带业务方法的 person 对象
;; 工厂函数 make-person-greet 返回一个闭包，被 openlet 标记后：
;; - 调用 (p :field) 时按 keyword 分派，返回字段值或业务结果
;; - 字段 (name age) 被闭包捕获，外部无法直接访问，实现封装
(define* (make-person-greet (name "?") (age 0))
  (openlet (lambda (key)
             (case key
              ((:name) name)
              ((:age) age)
              ((:greet) (format #f "Hi, I'm ~A, ~A years old." name age))
              (else (error 'unknown-method "person: ~A" key))
             ) ;case
           ) ;lambda
  ) ;openlet
) ;define*


;; (p :greet) 自动分发到业务方法
(check (let ((p (make-person-greet :name "Bob" :age 25)))
         (p :greet)
       ) ;let
  =>
  "Hi, I'm Bob, 25 years old."
) ;check


;; (p :name) / (p :age) 访问字段
(check (let ((p (make-person-greet :name "Alice" :age 30)))
         (list (p :name) (p :age))
       ) ;let
  =>
  '("Alice" 30)
) ;check


;; 默认参数：缺省时 name 为 "?"、age 为 0
(check (let ((p (make-person-greet))) (p :greet)) => "Hi, I'm ?, 0 years old.")


;; 应用示例：用 openlet 定义 matrix 对象
;; 工厂函数 make-matrix 返回一个被 openlet 标记的闭包：
;; - (m i j) 或 (apply m '(i j)) 取出第 i 行第 j 列的元素
;; - (m :rows) / (m :cols) 查询行列数
;; 内部用 s7 多维 vector 存储，vector-ref 原生支持多维索引
;;
;; 注：此处用 subvector 从一维 vector 构造 2x3 矩阵以避开 #2d(...)
;; 字面量（gf fmt 工具尚不支持该语法，会错误地将其重写为一维 vector）

(define (make-matrix data)
  (openlet (lambda args
             (case (length args)
                   ((1)
                    (case (car args)
                          ((:rows) (vector-dimension data 0))
                          ((:cols) (vector-dimension data 1))
                          (else (error 'unknown-method "matrix: ~A" (car args)))
                    ) ;case
                   ) ;
                   ((2) (apply vector-ref data args))
                   (else (error 'wrong-number-of-args "matrix expects 1 or 2 args, got ~A" (length args))
                   ) ;else
             ) ;case
           ) ;lambda
  ) ;openlet
) ;define


;; 构造 2x3 矩阵 [[1 2 3] [4 5 6]] 用于后续测试

(define test-matrix (subvector #(1 2 3 4 5 6) 0 6 '(2 3)))


;; (m i j) 直接取元素
(check (let ((m (make-matrix test-matrix))) (m 1 2)) => 6)


;; (apply m '(i j)) 也能取元素 —— 闭包原生支持 apply
(check (let ((m (make-matrix test-matrix))) (apply m '(1 2))) => 6)


;; (m :rows) / (m :cols) 查询维度
(check (let ((m (make-matrix test-matrix)))
         (list (m :rows) (m :cols))
       ) ;let
  =>
  '(2 3)
) ;check


;; 应用示例：用 openlet 模拟 data class（带 JSON 序列化）
;; 提供三层 API：
;; - make-person :name/:age 直接构造对象
;; - json->person  从 (liii json) 的 alist 构造
;; - string->person 从 JSON 字符串构造（内部 string->json 后委托 json->person）
;; 对象支持 :to-json 方法，导出为 (liii json) 兼容的 alist

(define (alist-get alist key)
  (let ((pair (assoc key alist)))
    (if pair (cdr pair) #f)
  ) ;let
) ;define

(define* (make-person (name "?") (age 0))
  (openlet (lambda (key)
             (case key
              ((:name) name)
              ((:age) age)
              ((:to-json) (list (cons "name" name) (cons "age" age)))
             ) ;case
           ) ;lambda
  ) ;openlet
) ;define*

(define (json->person j)
  (make-person :name (alist-get j "name") :age (alist-get j "age"))
) ;define

(define (string->person s)
  (json->person (string->json s))
) ;define


;; make-person :name/:age 直接构造
(check (let ((p (make-person :name "Bob" :age 25)))
         (list (p :name) (p :age))
       ) ;let
  =>
  '("Bob" 25)
) ;check


;; json->person 从 (liii json) 的 alist 构造对象
(check (let ((p (json->person (string->json "{\"name\":\"Alice\",\"age\":30}"))))
         (list (p :name) (p :age))
       ) ;let
  =>
  '("Alice" 30)
) ;check


;; string->person 从 JSON 字符串构造对象
(check (let ((p (string->person "{\"name\":\"Alice\",\"age\":30}")))
         (list (p :name) (p :age))
       ) ;let
  =>
  '("Alice" 30)
) ;check


;; :to-json 导出为字符串键 alist，可直接喂给 json->string
(check (let ((p (make-person :name "Bob" :age 25)))
         (p :to-json)
       ) ;let
  =>
  '(("name" . "Bob") ("age" . 25))
) ;check


;; 完整往返：JSON 字符串 -> person -> JSON 字符串
(check (let ((p (string->person "{\"name\":\"Alice\",\"age\":30}")))
         (json->string (p :to-json))
       ) ;let
  =>
  "{\"name\":\"Alice\",\"age\":30}"
) ;check


;; =========================================================================
;; inlet 与 openlet 的区别
;; =========================================================================
;;
;; 两者都能模拟面向对象，但机制不同：
;;
;;   inlet：对象就是环境（键值容器），(obj :key) 直接取绑定值。
;;          天然支持继承（outlet 链），但内建函数不会特殊处理它。
;;
;;   openlet：给环境/闭包打标记，使内建函数（length、object->string 等）
;;            遇到该对象时，查询其内部同名方法并调用（方法分派）。
;;
;; 关键差异：
;; ┌─────────────────┬──────────────────────┬───────────────────────┐
;; │ 维度            │ 纯 inlet              │ openlet 闭包          │
;; ├─────────────────┼──────────────────────┼───────────────────────┤
;; │ 对象本质        │ 环境（键值容器）      │ 闭包（lambda）        │
;; │ (obj :key)      │ 直接取绑定值          │ case 手动分派         │
;; │ 字段封装        │ 弱（绑定可见）        │ 强（闭包变量）        │
;; │ 继承            │ outlet 链天然支持     │ 需手动委托            │
;; │ 内建函数方法分派│ ✗ 不触发              │ ✓ 触发                │
;; └─────────────────┴──────────────────────┴───────────────────────┘
;;
;; 最后一行是核心区别：openlet 让对象能"改写"内建函数对它的处理方式，
;; inlet 做不到。下面用计数器对象演示两种实现。


;; ---- 纯 inlet 版计数器 -----------------------------------------
;; 对象是 inlet，字段/方法都是绑定，方法签名 (lambda (self) ...)
;; 状态 count 是闭包变量，多个实例状态独立

(define (make-counter-inlet)
  (let ((count 0))
    (inlet :get
      (lambda (self) count)
      :inc
      (lambda (self) (set! count (+ count 1)) count)
    ) ;inlet
  ) ;let
) ;define

(define ci (make-counter-inlet))

;; 方法调用：((ci :inc) ci) —— 取出方法后传 self
(check (begin ((ci :inc) ci) ((ci :inc) ci) ((ci :get) ci)) => 2)

;; 纯 inlet 的局限：length 不认识它，不会调用 :length 方法
;; 下面这个 inlet 定义了 :length，但 (length ci) 仍返回绑定数（2 个方法）
(check (length (inlet :length (lambda (self) 99) :get (lambda (self) 0))) => 2)


;; ---- openlet 版计数器 ------------------------------------------
;; 同样的逻辑，但用 openlet 包装后，length 会查询 :length 方法

(define (make-counter-openlet)
  (let ((count 0))
    (openlet (inlet :get
               (lambda (self) count)
               :inc
               (lambda (self) (set! count (+ count 1)) count)
               :length
               (lambda (self) count)
             ) ;inlet
    ) ;openlet
  ) ;let
) ;define

(define co (make-counter-openlet))
((co :inc) co)
((co :inc) co)

;; openlet 触发方法分派：length 查询到 :length 方法，返回 count 值
(check (length co) => 2)


;; ---- 封装性对比 -------------------------------------------------
;; inlet 的绑定对外可见：(ci :get) 直接返回方法 lambda
;; openlet 闭包模式（见前面 make-person）：字段是闭包变量，外部无法直接访问


(check-report)
