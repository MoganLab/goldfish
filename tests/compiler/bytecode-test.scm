(import (liii check)
        (liii base)
        (goldfish compiler ir)
        (goldfish compiler bytecode))

;; L2 字节码编译前端单元测试（record IR 版，词法寻址）。
;;
;; to-bytecode 把 IR record 树编译成可序列化指令序列（纯数据），
;; 是字节码 VM 后端的编译器前端。约定：
;;   尾位置应用 -> (tail-call n)；非尾 -> (call n)；
;;   本帧变量 -> (local i)；外层帧变量 -> (ref d i)；全局 -> (global name)。

;; 辅助：core sexp 列表 -> 字节码
(define (defs->bytecode defs)
  (to-bytecode (map core->ir defs)))

;; ===== 1. 简单 lambda：尾调用 + 全局引用 =====
(check (defs->bytecode '((define (add x) (+ x 1))))
       => '(program
            (code-table
             (code 1 (x) ((global +) (local 0) (const 1) (tail-call 2))))
            (top 0 (closure 0) (store-global add))))

;; ===== 2. if 分支与标签 =====
(check (defs->bytecode '((define (f x) (if (> x 0) 1 2))))
       => '(program
            (code-table
             (code 1 (x) ((global >) (local 0) (const 0) (call 2)
                          (if-else 0)
                          (const 1) (return)
                          (label 0)
                          (const 2) (return))))
            (top 0 (closure 0) (store-global f))))

;; ===== 3. let 槽分配 =====
(check (defs->bytecode '((define (f x) (let ((a (+ x 1))) (* a 2)))))
       => '(program
            (code-table
             (code 2 (x) ((global +) (local 0) (const 1) (call 2) (set-local 1)
                          (global *) (local 1) (const 2) (tail-call 2))))
            (top 0 (closure 0) (store-global f))))

;; ===== 4. 递归 + 尾调用 =====
(check (defs->bytecode '((define (loop i) (if (= i 0) 0 (loop (- i 1))))))
       => '(program
            (code-table
             (code 1 (i) ((global =) (local 0) (const 0) (call 2)
                          (if-else 0)
                          (const 0) (return)
                          (label 0)
                          (global loop) (global -) (local 0) (const 1) (call 2)
                          (tail-call 1))))
            (top 0 (closure 0) (store-global loop))))

;; ===== 5. begin 非尾表达式弹出 =====
(check (defs->bytecode '((define (f x) (begin (g x) (h x)))))
       => '(program
            (code-table
             (code 1 (x) ((global g) (local 0) (call 1) (pop)
                          (global h) (local 0) (tail-call 1))))
            (top 0 (closure 0) (store-global f))))

;; ===== 6. call-with-values 静态展开（producer 尾 values）=====
;; consumer 函数先压（栈序 [f a1 ... an]），values 作参数后压
(check (defs->bytecode '((define (f) (call-with-values
                                       (lambda () (values 1 2))
                                       (lambda (a b) (+ a b))))))
       => '(program
            (code-table
             (code 2 (a b) ((global +) (local 0) (local 1) (tail-call 2)))
             (code 0 () ((closure 0) (const 1) (const 2) (tail-call 2))))
            (top 0 (closure 1) (store-global f))))

;; ===== 7. call-with-values 通用（producer 非静态）=====
(check (defs->bytecode '((define (f g) (call-with-values g (lambda (a b) (+ a b))))))
       => '(program
            (code-table
             (code 2 (a b) ((global +) (local 0) (local 1) (tail-call 2)))
             (code 1 (g) ((local 0) (closure 0) (call-with-values) (return))))
            (top 0 (closure 1) (store-global f))))

;; ===== 8. 非 lambda 顶层 define =====
(check (defs->bytecode '((define v (list 1 2))))
       => '(program
            (code-table)
            (top 0 (global list) (const 1) (const 2) (call 2) (store-global v))))

;; ===== 9. 顶层表达式 =====
(check (defs->bytecode '((display "hi")))
       => '(program
            (code-table)
            (top 0 (global display) (const "hi") (call 1))))

;; ===== 10. 多值 values =====
(check (defs->bytecode '((define (f) (values 1 2))))
       => '(program
            (code-table
             (code 0 () ((const 1) (const 2) (values 2) (return))))
            (top 0 (closure 0) (store-global f))))

;; ===== 11. 嵌套 lambda：外层变量词法寻址 (ref 1 i) =====
(check (defs->bytecode '((define (f x) (lambda () x))))
       => '(program
            (code-table
             (code 0 () ((ref 1 0) (return)))
             (code 1 (x) ((closure 0) (return))))
            (top 0 (closure 1) (store-global f))))

;; ===== 12. 内部 define 转 letrec* 槽绑定 =====
(check (defs->bytecode '((define (f x) (define (g) x) (g))))
       => '(program
            (code-table
             (code 0 () ((ref 1 0) (return)))
             (code 2 (x) ((closure 0) (set-local 1) (local 1) (tail-call 0))))
            (top 0 (closure 1) (store-global f))))

;; ===== 13. 序列化往返：字节码是纯数据 =====
(check (let* ((bc (defs->bytecode '((define (f x) (if (> x 0) (f (- x 1)) 0))
                                    (define v '(1 2))
                                    (display v))))
              (s (call-with-output-string (lambda (p) (write bc p))))
              (bc2 (with-input-from-string s read)))
         (equal? bc bc2))
       => #t)

;; ===== 14. 结构校验：所有字节码合法 =====
(check (valid-bytecode? (defs->bytecode '((define (add x) (+ x 1)))))
       => #t)
(check (valid-bytecode? (defs->bytecode '((define (f x)
                                           (letrec ((loop (lambda (i)
                                                            (if (= i 0)
                                                              0
                                                              (loop (- i 1))))))
                                             (loop x))))))
       => #t)
(check (valid-bytecode? (defs->bytecode '((define (f) (call-with-values
                                                        (lambda () (values 1 2))
                                                        (lambda (a b) (+ a b)))))))
       => #t)
(check (valid-bytecode? (defs->bytecode '((define v (list 1 2)))))
       => #t)
(check (valid-bytecode? (defs->bytecode '((define (f x)
                                           (let ((a (let ((b (+ x 1))) (* b 2))))
                                             (+ a x))))))
       => #t)
(check (valid-bytecode? (defs->bytecode '((define (f) (values)))))
       => #t)

;; ===== 15. 边界补充 =====
;; 空 begin：不崩溃，压 #f
(check (defs->bytecode '((define (f) (begin))))
       => '(program
            (code-table
             (code 0 () ((const #f) (return))))
            (top 0 (closure 0) (store-global f))))

;; 非尾空 begin
(check (defs->bytecode '((define (f) (begin (begin) (f)))))
       => '(program
            (code-table
             (code 0 () ((const #f) (pop) (global f) (tail-call 0))))
            (top 0 (closure 0) (store-global f))))

;; if 无 else：else 分支压 #f
(check (defs->bytecode '((define (f x) (if x (f)))))
       => '(program
            (code-table
             (code 1 (x) ((local 0) (if-else 0)
                          (global f) (tail-call 0)
                          (label 0)
                          (const #f) (return))))
            (top 0 (closure 0) (store-global f))))

;; dotted formals：rest 参数收单个槽（formals 存原样符号）
(check (defs->bytecode '((define (f . rest) rest)))
       => '(program
            (code-table
             (code 1 rest ((local 0) (return))))
            (top 0 (closure 0) (store-global f))))

;; 局部 set! 与全局 set!
(check (defs->bytecode '((define (f x) (set! x 1))
                         (set! g 2)))
       => '(program
            (code-table
             (code 1 (x) ((const 1) (set-local 0) (return))))
            (top 0 (closure 0) (store-global f)
                 (const 2) (store-global g))))

(check-report)
