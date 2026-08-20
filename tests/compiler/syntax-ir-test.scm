(import (liii check)
        (goldfish compiler ir)
        (goldfish expander syntax-ir))

;; syntax->ir：展开后的 syntax 树直接转 IR record 树，binding-kind 保留。
;;
;; 与 core->ir 的区别：core->ir 从 lower 后的 core sexp 重建 IR，primitive
;; 引用只是裸符号，后端需靠名字匹配猜测；syntax->ir 在展开结果上重新
;; resolve-identifier，把 primitive 引用编码为 <primitive-ref> 节点。

;; ===== 1. 原子与自求值 =====
(check (expand->ir '42) => 42)
(check (expand->ir '"str") => "str")

;; ===== 2. primitive 引用 -> <primitive-ref> =====
(check (let ((ir (expand->ir '(car x))))
         (list (call? ir)
               (primitive-ref? (call-proc ir))
               (primitive-ref-name (call-proc ir))
               (call-args ir)))
       => '(#t #t car (x)))

;; 嵌套 primitive：外层 map 与内层 car 都是 primitive-ref
(check (let ((ir (expand->ir '(map (lambda (y) (car y)) xs))))
         (list (primitive-ref? (call-proc ir))
               (primitive-ref-name (call-proc ir))))
       => '(#t map))

;; ===== 3. 词法绑定保持 gensym =====
(check (let ((ir (expand->ir '(lambda (x) x))))
         (list (lambda? ir)
               (lambda-formals ir)
               (lambda-body ir)))
       => '(#t (x:1) (x:1)))

;; lambda 体内的 primitive 引用
(check (let ((ir (expand->ir '(lambda (x) (car x)))))
         (let ((b (car (lambda-body ir))))
           (list (call? b)
                 (primitive-ref? (call-proc b))
                 (primitive-ref-name (call-proc b))
                 (call-args b))))
       => '(#t #t car (x:1)))

;; ===== 4. 复合结构 =====
;; if
(check (let ((ir (expand->ir '(if (> x 0) 1 2))))
         (list (if? ir)
               (primitive-ref? (if-test ir))
               (if-then ir)
               (if-else ir)))
       => '(#t #f 1 2))

;; 无 else 的 if：expand-expr 以 (if #f #f) 补全 else，因此 if-else
;; 是空 if 树（与 core->ir 的 #f 语义等价：都求值为未指定值）
(check (let ((ir (expand->ir '(if x (f)))))
         (let ((e (if-else ir)))
           (list (if? ir)
                 (and (if? e) (eq? (if-test e) #f)))))
       => '(#t #t))

;; quote -> const
(check (let ((ir (expand->ir '(quote foo))))
         (list (const? ir) (const-value ir)))
       => '(#t foo))

;; 嵌套调用：call 的 proc 是 call
(check (let ((ir (expand->ir '((f x) y))))
         (list (call? ir)
               (call? (call-proc ir))
               (call-args ir)))
       => '(#t #t (y)))

;; ===== 5. 非 primitive 的顶层引用保持符号 =====
;; 未绑定标识符保持名字（resolve 返回 #f 时不标记 primitive）
(check (let ((ir (expand->ir '(apply1 f xs))))
         (list (call? ir)
               (symbol? (call-proc ir))
               (call-proc ir)))
       => '(#t #t apply1))
