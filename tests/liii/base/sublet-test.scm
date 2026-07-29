(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; sublet
;; 在已有 let 的环境内创建一个新的子 let，并初始化若干绑定。
;;
;; 语法
;; ----
;; (sublet parent-let)
;; (sublet parent-let symbol value ...)
;;
;; 参数
;; ----
;; parent-let : let?
;; 父环境（新 let 的 outlet 指向它）。
;;
;; symbol : symbol?
;; 绑定名。
;;
;; value : any
;; 绑定值。
;;
;; 返回值
;; ------
;; let?
;; 新创建的子 let。
;;
;; 说明
;; ----
;; sublet 创建的新 let 本身只包含新加入的绑定，
;; 但其 outlet 指向 parent-let，因此可通过环境链访问父绑定。
;; 这与 varlet 不同：varlet 把绑定直接合并进 target-let。


;; sublet 返回一个 let
(check (let? (sublet (inlet 'a 1))) => #t)


;; sublet 新 let 包含新加入的绑定
(check (let-ref (sublet (inlet 'a 1) 'b 2) 'b) => 2)


;; sublet 新 let 的 outlet 指向父 let
(check (let ((parent (inlet 'a 1)))
         (eq? (outlet (sublet parent 'b 2)) parent)
       ) ;let
  =>
  #t
) ;check


;; sublet 新 let 本身不包含父绑定（let->list 只显示直接绑定）
(check (let->list (sublet (inlet 'a 1) 'b 2)) => (list (cons 'b 2)))


;; sublet 新 let 长度只反映直接绑定数
(check (length (sublet (inlet 'a 1) 'b 2)) => 1)


;; 空 sublet 也返回 let
(check (let? (sublet (inlet))) => #t)


;; 应用示例：用 sublet + outlet 模拟继承
;; 设计要点：
;; - 类环境（inlet）作为方法载体，方法以符号为键，签名为 (lambda (self) ...)
;; - 子类用 (sublet 父类 (inlet ...)) 继承，覆盖的方法就近定义
;; - 实例用 (sublet 类 (inlet 字段...)) 构造，outlet 指向类环境
;; - 方法调用：((let-ref 实例 '方法名) 实例) —— let-ref 沿 outlet 链查找
;; - is-a 检查：遍历 outlet 链（到 rootlet 终止）判断实例是否属于某类

;; 基类 Animal

(define animal-class (inlet 'speak (lambda (self) "some sound")))

;; Dog 继承 Animal，覆盖 speak

(define dog-class (sublet animal-class (inlet 'speak (lambda (self) "woof"))))

;; Cat 继承 Animal，覆盖 speak

(define cat-class (sublet animal-class (inlet 'speak (lambda (self) "meow"))))

;; is-a 检查：遍历 outlet 链找目标类

(define (subclass-of? instance-let target-class)
  (let loop
    ((e instance-let))
    (cond ((eq? e (rootlet)) #f)
          ((not (let? e)) #f)
          ((eq? e target-class) #t)
          (else (loop (outlet e)))
    ) ;cond
  ) ;let
) ;define

;; 实例工厂

(define (make-animal name)
  (sublet animal-class (inlet 'name name))
) ;define

(define (make-dog name)
  (sublet dog-class (inlet 'name name))
) ;define

(define (make-cat name)
  (sublet cat-class (inlet 'name name))
) ;define


;; 方法覆盖：Dog.speak 返回 "woof"
(check (let ((d (make-dog "Rex"))) ((let-ref d 'speak) d)) => "woof")


;; 方法覆盖：Cat.speak 返回 "meow"
(check (let ((c (make-cat "Whiskers"))) ((let-ref c 'speak) c)) => "meow")


;; 未覆盖的方法继承自基类：Animal.speak 返回 "some sound"
(check (let ((a (make-animal "generic")))
         ((let-ref a 'speak) a)
       ) ;let
  =>
  "some sound"
) ;check


;; is-a 检查：Dog 实例既是 dog 也是 animal
(check (let ((d (make-dog "Rex")))
         (list (subclass-of? d dog-class)
           (subclass-of? d animal-class)
           (subclass-of? d cat-class)
         ) ;list
       ) ;let
  =>
  '(#t #t #f)
) ;check


(check-report)
