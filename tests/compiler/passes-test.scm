(import (liii check)
        (goldfish match)
        (goldfish core ir)
        (goldfish compiler patterns)
        (goldfish compiler passes))

;; pass 管线（record IR 版）单元测试。

;; 辅助：把 core sexp 转换 + pass + 转回 sexp
(define (fold-sexp core pass)
  (ir->core (pass (core->ir core))))

;; ===== 1. constant-fold：常量调用折叠 =====
(check (fold-sexp '(+ 1 2) constant-fold) => 3)
(check (fold-sexp '(* 2 3) constant-fold) => 6)
(check (fold-sexp '(= 1 1) constant-fold) => #t)
(check (fold-sexp '(string-append "a" "b") constant-fold) => "ab")

;; 非常量参数不折叠
(check (fold-sexp '(+ x 1) constant-fold) => '(+ x 1))

;; 嵌套折叠
(check (fold-sexp '(+ (+ 1 2) 3) constant-fold) => 6)

;; 运行时错误不折叠（保留原调用）
(check (fold-sexp '(quotient 1 0) constant-fold) => '(quotient 1 0))

;; quote 数据作为常量进入折叠
(check (fold-sexp '(car (quote (1 2))) constant-fold) => '(car (quote (1 2))))

;; ===== 2. simplify-if：布尔测试化简 =====
(check (fold-sexp '(if #t 1 2) simplify-if) => 1)
(check (fold-sexp '(if #f 1 2) simplify-if) => 2)
(check (fold-sexp '(if x 1 2) simplify-if) => '(if x 1 2))

;; 测试表达式先化简（配合 constant-fold）
(check (let ((ir (run-passes (core->ir '(if (= 1 1) a b))
                             (list constant-fold simplify-if))))
         (ir->core ir))
       => 'a)

;; 单独 simplify-if 不折叠调用（保留原样）
(check (fold-sexp '(if (= 1 1) a b) simplify-if) => '(if (= 1 1) a b))

;; ===== 3. 组合 pass：先折叠再化简 =====
(check (let ((ir (run-passes (core->ir '(if (> 3 2) (+ 1 2) 0))
                             (list constant-fold simplify-if))))
         (ir->core ir))
       => 3)

;; ===== 4. 结构保持：pass 输出仍是 record 树 =====
(check (let ((ir (constant-fold (core->ir '(lambda (x) (+ x 1))))))
         (lambda? ir))
       => #t)

(check (let ((ir (simplify-if (core->ir '(if x 1 2)))))
         (conditional? ir))
       => #t)

;; ===== 5. inline：copy propagation + beta reduction =====
;; 常量传播
(check (fold-sexp '(let ((a 1)) (+ a 2)) inline) => '(+ 1 2))

;; lambda 内联（beta reduction）
(check (fold-sexp '((lambda (x) (+ x 1)) 5) inline) => '(+ 5 1))

;; 递归函数不被传播
(check (fold-sexp '(letrec ((loop (lambda (i) (if (= i 0) 0 (loop (- i 1))))))
                     (loop 3))
                   inline)
       => '(letrec ((loop (lambda (i) (if (= i 0) 0 (loop (- i 1))))))
            (loop 3)))

;; 未引用的绑定被剪除
(check (fold-sexp '(let ((a 1) (b 2)) b) inline) => '2)

;; lambda 传播后应用 beta-reduce
(check (fold-sexp '(let ((f (lambda (x) (+ x 1)))) (f 2)) inline) => '(+ 2 1))

;; ===== 6. tail-call-positions =====
;; 尾位置应用被 (tail-call ...) 包装。lambda body 是 <lambda-case>，
;; 先经 lambda-body 取 case、再取 case-body 表达式。
(check (let* ((ir (tail-call-positions (core->ir '(lambda (x) (if (> x 0) (f x) (g x))))))
              (body (lambda-case-body (lambda-body ir))))
         (match body
           (($conditional test (tail-call th) (tail-call el))
            (list (ir->core th) (ir->core el)))
           (_ 'no)))
       => '((f x) (g x)))

;; 尾应用被标记，内层嵌套调用 (f x) 不标记
(check (let* ((ir (tail-call-positions (core->ir '(lambda (x) (+ 1 (f x))))))
              (body (lambda-case-body (lambda-body ir))))
         (match body
           ((tail-call c)
            (list (ir->core (call-proc c))
                  (ir->core (cadr (call-args c)))))
           (_ 'no)))
       => '(+ (f x)))

;; ===== 7. eliminate-dead-defs =====
;; 未引用的 lambda def 删除；used 被 main 引用保留，main 被 register 保留
(check (let* ((defs (map core->ir '((define used (lambda (x) (+ x 1)))
                                     (define unused (lambda (y) (* y 2)))
                                     (define main (lambda () (used 1)))
                                     (register main))))
              (survivors (eliminate-dead-defs defs)))
         (map (lambda (d) (toplevel-define-name d)) survivors))
       => '(used main register))

;; 引用链保留：register 引用 b，b 引用 a
(check (let* ((defs (map core->ir '((define a (lambda () 1))
                                     (define b (lambda () (a)))
                                     (register b))))
              (survivors (eliminate-dead-defs defs)))
         (map (lambda (d) (toplevel-define-name d)) survivors))
       => '(a b register))

;; 自我递归的 def 若被引用保留
(check (let* ((defs (map core->ir '((define loop (lambda (i) (if (= i 0) 0 (loop (- i 1)))))
                                     (register loop))))
              (survivors (eliminate-dead-defs defs)))
         (map (lambda (d) (toplevel-define-name d)) survivors))
       => '(loop register))

(check-report)
