(import (liii check) (goldfish))

;; quasisyntax / unsyntax / unsyntax-splicing 测试。
;;
;; reader 已把 #` / #, / #,@ 读成 (quasisyntax ...) / (unsyntax ...) /
;; (unsyntax-splicing ...)；expander 的 core-quasisyntax 把模板翻译成
;; 构造 syntax 对象的表达式（静态部分 quote-syntax，动态 unsyntax 展开）。

;; ===== 1. 静态模板（无 unsyntax）=====
(check (let-syntax ((m (lambda (stx) (syntax-case stx () ((_) #`(list 1 2))))))
          (m))
       => '(1 2))

;; ===== 2. unsyntax：pattern 变量直接引用 =====
(check (let-syntax ((m (lambda (stx)
                         (syntax-case stx ()
                           ((_ x) #`(list #,x))))))
          (m 7))
       => '(7))

;; ===== 3. unsyntax：with-syntax 绑定的 syntax 对象 =====
(check (let-syntax ((m (lambda (stx)
                         (syntax-case stx ()
                           ((_ x) (with-syntax ((y #'x))
                                    #`(list #,y)))))))
          (m 7))
       => '(7))

;; ===== 4. 多个 unsyntax =====
(check (let-syntax ((m (lambda (stx)
                         (syntax-case stx ()
                           ((_ a b) (with-syntax ((va #'a) (vb #'b))
                                      #`(list #,va #,vb)))))))
          (m 1 2))
       => '(1 2))

;; ===== 5. 自由标识符保留词法引用 =====
(check (let-syntax ((m (lambda (stx)
                         (syntax-case stx ()
                           ((_) #`(list + 1 2))))))
          (let ((+ -))
            (car (m))))
       => +)

;; ===== 6. 已知缺口 =====
;; 6a. (syntax X) 子模板：产 syntax 对象（datum 嵌 syntax 值），展开器保留
;;     为值（Racket 语义）；用户 syntax->datum 递归得 X 的 datum。
(check (syntax->datum (let-syntax ((m (lambda (stx)
                                        (syntax-case stx ()
                                          ((_) (quasisyntax (list 1 (syntax (lit 2)))))))))
                         (m)))
       => '(1 (lit 2)))
;; 6b. 模板里裸标识符引用 with-syntax/pattern 绑定（如 `(list v)` 而非
;;     `(list #,v)`）：Racket 语义 —— pattern 变量替换为值，自由标识符
;;     保留词法引用，普通词法绑定（let）不替换（运行时 unbound）。
(check (let-syntax ((m (lambda (stx)
                         (syntax-case stx ()
                           ((_ x) #`(list x))))))
          (m 7))
       => '(7))
(check (let-syntax ((m (lambda (stx)
                         (syntax-case stx ()
                           ((_ x) (with-syntax ((y #'x))
                                    #`(list y x)))))))
          (m 7))
       => '(7 7))
;; 6c. 嵌套 quasisyntax（(quasisyntax (quasisyntax ...))）：当前按字面保留。

(check-report)
