(import (liii check)
        (goldfish compiler ir)
        (goldfish compiler passes)
        (goldfish compiler bytecode)
        (goldfish compiler syntax-ir))

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
               (toplevel-ref-name (car (call-args ir)))))
       => '(#t #t car x))

;; 嵌套 primitive：外层 map 与内层 car 都是 primitive-ref
(check (let ((ir (expand->ir '(map (lambda (y) (car y)) xs))))
         (list (primitive-ref? (call-proc ir))
               (primitive-ref-name (call-proc ir))))
       => '(#t map))

;; ===== 3. 词法绑定 -> <lexical-ref>（depth/index 前置）=====
;; (lambda (x) x)：body 引用 x，depth 0 index 0
(check (let ((ir (expand->ir '(lambda (x) x))))
         (let ((b (car (lambda-body ir))))
           (list (lambda? ir)
                 (lambda-formals ir)
                 (lexical-ref? b)
                 (lexical-ref-depth b)
                 (lexical-ref-index b))))
       => '(#t (x:1) #t 0 0))

;; lambda 体内的 primitive 引用 + 词法参数
(check (let ((ir (expand->ir '(lambda (x) (car x)))))
         (let ((b (car (lambda-body ir))))
           (list (call? b)
                 (primitive-ref? (call-proc b))
                 (primitive-ref-name (call-proc b))
                 (let ((a (car (call-args b))))
                   (list (lexical-ref? a)
                         (lexical-ref-depth a)
                         (lexical-ref-index a))))))
       => '(#t #t car (#t 0 0)))

;; 嵌套 lambda：内层引用外层变量 depth 1
(check (let ((ir (expand->ir '(lambda (x) (lambda (y) x)))))
         (let ((inner (car (lambda-body ir))))
           (let ((b (car (lambda-body inner))))
             (list (lexical-ref? b)
                   (lexical-ref-depth b)
                   (lexical-ref-index b)))))
       => '(#t 1 0))

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
(check (if? (if-else (expand->ir '(if x (f)))))
       => #t)
(check (eq? (if-test (if-else (expand->ir '(if x (f))))) #f)
       => #t)

;; quote -> const
(check (let ((ir (expand->ir '(quote foo))))
         (list (const? ir) (const-value ir)))
       => '(#t foo))

;; 嵌套调用：call 的 proc 是 call
(check (let ((ir (expand->ir '((f x) y))))
         (list (call? ir)
               (call? (call-proc ir))
               (toplevel-ref-name (car (call-args ir)))))
       => '(#t #t y))

;; ===== 5. 非 primitive 的顶层引用保持名字 =====
;; 未绑定标识符保持名字（resolve 返回 #f 时不标记 primitive）
(check (let ((ir (expand->ir '(apply1 f xs))))
         (list (call? ir)
               (toplevel-ref? (call-proc ir))
               (toplevel-ref-name (call-proc ir))))
       => '(#t #t apply1))

;; ===== 6. compile-syntax-defs：库定义管线 =====
;; syntax defs -> IR -> passes -> lowered sexp，与 core->ir 路径输出等价。
(check (let*-values (((defs ctx) (expand-library-body
                                  (list (wrap-expression '(define (f) (+ 1 2))))
                                  the-base-library
                                  (initial-context))))
         (equal? (compile-syntax-defs defs ctx (list constant-fold simplify-if))
                 (compile-defs (map lower defs) (list constant-fold simplify-if))))
       => #t)

;; primitive-ref 参与常量折叠：(+ 1 2) -> 3
(check (let*-values (((defs ctx) (expand-library-body
                                  (list (wrap-expression '(define (f) (+ 1 2))))
                                  the-base-library
                                  (initial-context))))
         (let ((out (compile-syntax-defs defs ctx (list constant-fold simplify-if))))
           (equal? out '((define f:0 (lambda () 3))))))
       => #t)

;; ===== 7. 词法寻址前置 =====
;; 词法引用产 <lexical-ref> 节点，depth/index 在展开层计算
(check (let ((ir (expand->ir '(lambda (x) x))))
         (let ((b (car (lambda-body ir))))
           (list (lexical-ref? b)
                 (lexical-ref-depth b)
                 (lexical-ref-index b))))
       => '(#t 0 0))

;; 双参数
;; 双参数：list 是 primitive-ref，args 是词法引用
(check (let ((ir (expand->ir '(lambda (x y) (list x y)))))
         (let ((call (car (lambda-body ir))))
           (list (primitive-ref? (call-proc call))
                 (primitive-ref-name (call-proc call))
                 (map (lambda (a)
                        (list (lexical-ref-depth a) (lexical-ref-index a)))
                      (call-args call)))))
       => '(#t list ((0 0) (0 1))))

;; 嵌套 lambda：内层引用外层变量 -> depth 1
(check (let ((ir (expand->ir '(lambda (x) (lambda (y) x)))))
         (let ((inner (car (lambda-body ir))))
           (let ((b (car (lambda-body inner))))
             (list (lexical-ref? b)
                   (lexical-ref-depth b)
                   (lexical-ref-index b)))))
       => '(#t 1 0))

;; s7 eval 路径：compile-syntax-defs 输出词法保持符号（无 lexical-ref）
(check (let*-values (((defs ctx) (expand-library-body
                                  (list (wrap-expression '(define (sq x) (* x x))))
                                  the-base-library
                                  (initial-context))))
         (let ((out (compile-syntax-defs defs ctx '())))
           (equal? out '((define sq:0 (lambda (x:2) (* x:2 x:2)))))))
       => #t)

;; ===== 8. lexical-ref 字节码编译 =====
;; （to-bytecode 编译已在 syntax-vm-e2e-test 的端到端闭环中覆盖）

;; ===== 9. rest formals 保持 dotted（回归）=====
;; formals->datum 曾把 (l . r) 展平成 (l r)，使 lambda-case-rest 丢失，
;; ir->core 还原出 (lambda (l r) ...) 而非 (lambda (l . r) ...)，进而破坏
;; srfi-13 的 string-join 等按 rest 调用的函数（extract-params 收到裸分隔符）。
(check (let*-values (((defs ctx) (expand-library-body
                                  (list (wrap-expression '(define (f l . r) (car r))))
                                  the-base-library
                                  (initial-context))))
         (let* ((ir (syntax->ir/sexp (car defs) ctx))
                (core (ir->core ir))
                (formals (cadr (caddr core))))
           ;; (lambda (l . r) ...) restores a dotted formals list; the
           ;; flattened (l r) regression would fail the symbol? tail check.
           (and (pair? formals) (symbol? (cdr formals)))))
       => #t)
