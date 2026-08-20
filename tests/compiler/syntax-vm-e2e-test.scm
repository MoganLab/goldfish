(import (liii check)
        (goldfish)
        (goldfish compiler ir)
        (goldfish compiler bytecode)
        (goldfish compiler passes)
        (goldfish expander syntax-ir))

;; 端到端闭环测试：expander 展开 → syntax->ir（含 primitive-ref/lexical-ref）
;; → to-bytecode → VM 执行。
;;
;; 与 vm-transformer-test（core->ir 手写 sexp）的区别：本测试从 expander
;; 的真实产物（syntax 对象）出发，覆盖 syntax->ir 的完整路径——binding
;; 访问、词法槽位分配、letrec 闭包捕获、Scheme map 回调等。

(define (vm-load-syntax exprs)
  (let*-values (((defs ctx) (expand-library-body
                             (map wrap-expression exprs)
                             the-base-library
                             (initial-context))))
    (let ((irs (map (lambda (d) (syntax->ir d ctx)) defs)))
      (vm-load (to-bytecode irs) #f)
      irs)))

;; ===== 1. 递归 + 条件 + 算术 =====
(define irs1 (vm-load-syntax
              '((define (a-fact n)
                  (if (= n 0) 1 (* n (a-fact (- n 1))))))))
(check (eval (list (define-name (car irs1)) 5) (rootlet)) => 120)
(check (eval (list (define-name (car irs1)) 0) (rootlet)) => 1)

;; ===== 2. Scheme map + VM closure 回调 =====
(define irs2 (vm-load-syntax
              '((define (a-double l)
                  (map (lambda (x) (* x 2)) l))
                (define (a-add2 a b)
                  (map + a b)))))
(check (eval (list (define-name (car irs2)) (list 'quote '(1 2 3))) (rootlet)) => '(2 4 6))
(check (eval (list (define-name (cadr irs2))
                   (list 'quote '(1 2)) (list 'quote '(10 20)))
             (rootlet))
       => '(11 22))

;; ===== 3. letrec 闭包 + 递归 + 列表构造（槽位分配）=====
(define irs3 (vm-load-syntax
              '((define (a-count n)
                  (letrec ((loop (lambda (i acc)
                                   (if (= i 0) acc (loop (- i 1) (cons i acc))))))
                    (loop n '()))))))
(check (eval (list (define-name (car irs3)) 4) (rootlet)) => '(1 2 3 4))

;; ===== 4. 嵌套 lambda 捕获 =====
(define irs4 (vm-load-syntax
              '((define (a-nest x)
                  (lambda (y) (+ x y))))))
(check ((eval (list (define-name (car irs4)) 10) (rootlet)) 5) => 15)

;; ===== 5. 宏展开形态（真实库代码）：cond / let* / and / or / when / unless =====
(define irs-mac (vm-load-syntax
                 '((define (a-cond x)
                     (cond ((< x 0) 'neg)
                           ((= x 0) 'zero)
                           (else 'pos)))
                   (define (a-let* a b)
                     (let* ((x (+ a 1))
                            (y (* x b)))
                       (list x y)))
                   (define (a-and a b)
                     (and (> a 0) (> b 0)))
                   (define (a-when x)
                     (when (> x 0) (list 'pos x))))))
(check (eval (list (define-name (car irs-mac)) -1) (rootlet)) => 'neg)
(check (eval (list (define-name (car irs-mac)) 0) (rootlet)) => 'zero)
(check (eval (list (define-name (car irs-mac)) 5) (rootlet)) => 'pos)
(check (eval (list (define-name (cadr irs-mac)) 2 3) (rootlet)) => '(3 9))
(check (eval (list (define-name (caddr irs-mac)) 1 2) (rootlet)) => #t)
(check (eval (list (define-name (caddr irs-mac)) 1 -1) (rootlet)) => #f)
(check (eval (list (define-name (cadddr irs-mac)) 3) (rootlet)) => '(pos 3))

;; ===== 6. do 循环（→ letrec loop）=====
(define irs-do (vm-load-syntax
                '((define (a-do n)
                    (do ((i 0 (+ i 1))
                         (acc '() (cons i acc)))
                        ((= i n) (reverse acc)))))))
(check (eval (list (define-name (car irs-do)) 4) (rootlet)) => '(0 1 2 3))

;; ===== 7. case =====
(define irs-case (vm-load-syntax
                  '((define (a-case x)
                      (case x
                        ((1 2) 'small)
                        ((3 4) 'medium)
                        (else 'large))))))
(check (eval (list (define-name (car irs-case)) 1) (rootlet)) => 'small)
(check (eval (list (define-name (car irs-case)) 4) (rootlet)) => 'medium)
(check (eval (list (define-name (car irs-case)) 9) (rootlet)) => 'large)

;; ===== 8. 字符串 + map（真实库场景）=====
(define irs-str (vm-load-syntax
                 '((define (a-toupper s)
                     (list->string (map char-upcase (string->list s)))))))
(check (eval (list (define-name (car irs-str)) (list 'quote "abc")) (rootlet)) => "ABC")

;; ===== 9. 真实库代码锚点（抽取 (liii list) 的自包含纯函数）=====
(define (qv . xs) (list 'quote xs))

;; length=? ：递归 + cond 多分支
(define irs-len (vm-load-syntax
                 '((define (length=? x scheme-list)
                     (cond ((and (= x 0) (null? scheme-list)) #t)
                           ((or (= x 0) (null? scheme-list)) #f)
                           (else (length=? (- x 1) (cdr scheme-list))))))))
(check (eval (list (define-name (car irs-len)) 3 (qv 'a 'b 'c)) (rootlet)) => #t)
(check (eval (list (define-name (car irs-len)) 2 (qv 'a 'b 'c)) (rootlet)) => #f)

;; length>? ：let loop + cond
(define irs-gt (vm-load-syntax
                '((define (length>? lst len)
                    (let loop
                      ((lst lst) (cnt 0))
                      (cond ((null? lst) (< len cnt))
                            ((pair? lst) (loop (cdr lst) (+ cnt 1)))
                            (else (< len cnt))))))))
(check (eval (list (define-name (car irs-gt)) (qv 1 2 3 4) 2) (rootlet)) => #t)
(check (eval (list (define-name (car irs-gt)) (qv 1 2) 5) (rootlet)) => #f)

;; list-drop ：unless/cond + let loop
(define irs-drop (vm-load-syntax
                  '((define (list-drop lst n)
                      (cond ((< n 0) lst)
                            ((= n 0) lst)
                            (else (let loop
                                    ((rest lst) (count 0))
                                    (cond ((null? rest) '())
                                          ((>= count n) rest)
                                          (else (loop (cdr rest) (+ count 1)))))))))))
(check (eval (list (define-name (car irs-drop)) (qv 'a 'b 'c 'd) 2) (rootlet)) => '(c d))
(check (eval (list (define-name (car irs-drop)) (qv 'a 'b) 0) (rootlet)) => '(a b))
(check (eval (list (define-name (car irs-drop)) (qv 'a 'b) 5) (rootlet)) => '())

;; list-null? / list-not-null? ：and/or
(define irs-null (vm-load-syntax
                  '((define (list-null? l)
                      (and (not (pair? l)) (null? l)))
                    (define (list-not-null? l)
                      (and (pair? l) (or (null? (cdr l)) (pair? (cdr l))))))))
(check (eval (list (define-name (car irs-null)) '()) (rootlet)) => #t)
(check (eval (list (define-name (car irs-null)) (qv 'a)) (rootlet)) => #f)
(check (eval (list (define-name (cadr irs-null)) (qv 'a)) (rootlet)) => #t)

;; ===== 10. flatten：内部 define + set-cdr! 共享结构 + 嵌套递归 =====
(define irs-flat
  (vm-load-syntax
   '((define (flatten lst)
       (define (flatten-depth-iter rest depth res-node)
         (if (null? rest)
           res-node
           (let ((first (car rest)) (tail (cdr rest)))
             (cond ((and (null? first) (not (= 0 depth)))
                    (flatten-depth-iter tail depth res-node))
                   ((or (= depth 0) (not (pair? first)))
                    (set-cdr! res-node (cons first '()))
                    (flatten-depth-iter tail depth (cdr res-node)))
                   (else (flatten-depth-iter tail depth
                                             (flatten-depth-iter first (- depth 1) res-node)))))))
       (define (flatten-depth lst depth)
         (let ((res (cons #f '())))
           (flatten-depth-iter lst depth res)
           (cdr res)))
       (flatten-depth lst 1)))))
(check (eval (list (define-name (car irs-flat)) (qv 1 '(2 3) 4)) (rootlet)) => '(1 2 3 4))
(check (eval (list (define-name (car irs-flat)) (qv '(1 2) '(3 4))) (rootlet)) => '(1 2 3 4))

;; ===== 11. VM 库加载路径：vm-load-syntax-defs（含 passes）=====
;; 库代码经 syntax->ir → passes → bytecode → vm-load 到 the-expander-library
;; （s7 eval 路径的同款环境），store-global 的 gensym 值可经 eval 取回调用。
(let*-values (((defs ctx) (expand-library-body
                           (map wrap-expression
                                '((define (vm-len=? x l)
                                    (cond ((and (= x 0) (null? l)) #t)
                                          ((or (= x 0) (null? l)) #f)
                                          (else (vm-len=? (- x 1) (cdr l)))))
                                  (define (vm-drop lst n)
                                    (cond ((< n 0) lst)
                                          ((= n 0) lst)
                                          (else (let loop
                                                  ((rest lst) (count 0))
                                                  (cond ((null? rest) '())
                                                        ((>= count n) rest)
                                                        (else (loop (cdr rest) (+ count 1))))))))))
                           the-base-library
                           (initial-context))))
  (let ((irs (vm-load-syntax-defs defs ctx
                                  (list constant-fold simplify-if)
                                  the-expander-library)))
    (check ((eval (define-name (car irs)) the-expander-library)
            3 (list 'a 'b 'c))
           => #t)
    (check ((eval (define-name (cadr irs)) the-expander-library)
            (list 'a 'b 'c 'd) 2)
           => '(c d))))
