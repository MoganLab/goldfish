(import (liii check) (goldfish))

;; 宏系统高级特性覆盖（测试先行：先测现状，暴露缺口再补实现）。
;;
;; 已实现：自托管 syntax-case（对象级宏）、syntax-rules、with-syntax、
;; 模板预编译、guard/fender、vector/dotted ellipsis、define-macro。
;; 已知缺口（记录于测试尾部的 check-false 断言）：
;;   - 命名/自定义 ellipsis（字面量表列 `...`）
;;   - ellipsis 转义（(... ...)）
;;   - quasisyntax / unsyntax / unsyntax-splicing

;; ===== 1. nested ellipsis（双层）=====
;; 现状：syntax-rules 的嵌套 ellipsis 模式在定义层就报
;; "syntax-case: no matching clause"（pattern-match* 不支持 `...` 内嵌）。
;; 记录为已知缺口；阶段3 实现 nested ellipsis 后启用下方正例。
(check (catch #t
         (lambda ()
           (eval '(define-syntax nested-map
                    (syntax-rules ()
                      ((_ ((x y) ...) ...) (list (list x y) ...) ...))))
           (eval '(nested-map (1 2) (3 4))))
         (lambda (tag . info) 'expansion-error))
       => 'expansion-error)

;; ===== 2. syntax-case guard/fender =====
(define-syntax guarded
  (lambda (stx)
    (syntax-case stx ()
      ((_ x)
       (identifier? #'x)
       #'(quote identifier))
      ((_ x)
       #'(quote other)))))
(check (guarded foo) => 'identifier)
(check (guarded (1 2)) => 'other)

;; guard 分支拒绝非标识符并尝试下一子句
(define-syntax must-be-id
  (lambda (stx)
    (syntax-case stx ()
      ((_ x) (not (identifier? #'x)) #'(quote bad))
      ((_ x) #'(quote good)))))
(check (must-be-id foo) => 'good)
(check (must-be-id 42) => 'bad)

;; ===== 3. with-syntax =====
(define-syntax with-ws
  (lambda (stx)
    (syntax-case stx ()
      ((_ x)
       (with-syntax ((y #'x) (z #'(list 1 2)))
         #'(list y (car z)))))))
(check (with-ws 10) => '(10 1))

;; ===== 4. generate-temporaries：防捕获 =====
;; 已知缺口：generate-temporaries 返回的 identifier 本身合法（identifier? #t），
;; 但模板实例化在绑定位置把它双包装（外层 syntax 的 form 变成 syntax 对象
;; 而非 symbol），let->lambda 报 "lambda: expected identifier"（阶段3 修模板
;; 实例化后启用：safe-swap / temp-shadows 正例）。

;; ===== 5. free-identifier=?：字面量匹配 =====
(define-syntax literal-check
  (lambda (stx)
    (syntax-case stx (else)
      ((_ else) #'(quote matched))
      ((_ x) #'(quote unmatched)))))
(check (literal-check else) => 'matched)
(check (literal-check other) => 'unmatched)

;; ===== 6. define-macro（非卫生）=====
(define-macro (non-hygienic)
  `(list original-value))
(let ((original-value 42))
  (check (non-hygienic) => '(42)))

;; ===== 7. let-syntax / letrec-syntax =====
(let-syntax ((double (syntax-rules () ((_ x) (* 2 x)))))
  (check (double 21) => 42))
;; 已知缺口：letrec-syntax 递归宏 hang（(fact 5) 展开死循环，进程无输出超时）。
;; 阶段3 排查 letrec-syntax 的递归绑定后启用：(fact 5) => 120。

;; ===== 8. quote-syntax 保留词法上下文 =====
;; 已知缺口：quote-syntax 在表达式位置求值为 datum（foo）而非 syntax 对象
;; （lower 渲染为 (quote <datum>) 时丢 syntax 身份），(syntax? (quote-syntax x))
;; 为 #f。阶段3 修 lower 后启用下方正例。
(define-syntax qs
  (lambda (stx)
    (syntax-case stx ()
      ((_ x) (with-syntax ((q (quote-syntax x)))
               #'(quote q))))))
(check (syntax? (qs whatever)) => #f)

;; ===== 9. 已知缺口探测 =====
;; 9a. 命名 ellipsis：字面量表列 `...` 应允许别名 ellipsis。
;; R7RS portable match-check-ellipsis 惯用法。当前实现不支持。
(check (catch #t
         (lambda ()
           (eval '(let-syntax ((m (syntax-rules (...) ((_ ...) 'ok))))
                    (m x y z))))
         (lambda args 'expansion-error))
       => 'expansion-error)

;; 9b. ellipsis 转义：(... ...) 应产出一个字面 ...（R6RS）。当前不支持。
(check (catch #t
         (lambda ()
           (eval '(let-syntax ((m (syntax-rules () ((_ x) '(... ...)))))
                    (m 1))))
         (lambda args 'expansion-error))
       => 'expansion-error)

;; 9c. quasisyntax / unsyntax：当前完全缺失（reader 可读 #`/#, 但无法展开）。
(check (catch #t
         (lambda ()
           (eval '(let-syntax ((m (lambda (stx)
                                    (syntax-case stx ()
                                      ((_ x) (with-syntax ((y #'x))
                                               #`(list #,y)))))))
                    (m 7))))
         (lambda args 'expansion-error))
       => 'expansion-error)

(check-report)
