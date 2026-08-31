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
;; 6a. (syntax X) 子模板：datum 嵌 syntax 对象，goldfish 展开器递归展开
;;     （当程序，展开期崩溃），Racket 保留 datum 为值。需展开器支持
;;     datum 嵌 syntax（quote-syntax 语义）。
;; 6b. 模板里裸标识符引用 with-syntax/pattern 绑定（如 `(list v)` 而非
;;     `(list #,v)`）：当前保留为自由词法引用（运行时未绑定）。Racket 的
;;     pattern 变量替换语义尚未在 quasisyntax 中实现。
;; 6c. 嵌套 quasisyntax（(quasisyntax (quasisyntax ...))）：当前按字面保留。

(check-report)
