(import (liii check) (goldfish))

;; R7RS named ellipsis： (syntax-rules <ellipsis> (lit ...) (pat tmpl) ...)
;; 用 <ellipsis> 作为 pattern/template 的 ellipsis 标记（Guile 同样支持）。
;; 实现：syntax-rules 展开时把 <ellipsis> 替换成默认 `...'，运行时不变。

;; ===== 1. 基本：自定义 ellipsis ::: =====
(define-syntax my-and
  (syntax-rules ::: ()
    ((_) #t)
    ((_ x :::) (and x :::))))
(check (my-and) => #t)
(check (my-and #t #t) => #t)
(check (my-and #t #t #t) => #t)

;; ===== 2. 嵌套绑定 + 模板 ellipsis =====
(define-syntax my-let*
  (syntax-rules ::: ()
    ((_ ((x v) :::) body :::)
     (let ((x v) :::) body :::))))
(check (my-let* ((a 1) (b 2) (c 3)) (+ a b c)) => 6)
(check (my-let* () 42) => 42)

;; ===== 3. 其他自定义标记（Guile 用 :: 或类似）=====
(define-syntax collect
  (syntax-rules :: ()
    ((_ x ::) (list x ::))))
(check (collect 1 2 3) => '(1 2 3))

;; ===== 4. 与默认 ellipsis 并存（互不影响）=====
;; 默认 ellipsis 的宏仍工作。
(check (let-syntax ((m (syntax-rules () ((_ x ...) (list x ...)))))
          (m 1 2 3))
       => '(1 2 3))

;; ===== 5. 字面量 `...'（syntax-rules (...)）=====
;; `...' 列在字面量表列时匹配字面 `...' 标识符（非 ellipsis），
;; R7RS portable match-check-ellipsis 惯用法（Guile 同样支持）。
(define-syntax match-check-ellipsis
  (syntax-rules (...)
    ((_ ... sk fk) sk)
    ((_ x sk fk) fk)))
(check (match-check-ellipsis ... 'success 'failure) => 'success)
(check (match-check-ellipsis (a . b) 'success 'failure) => 'failure)
(check (match-check-ellipsis #(a b) 'success 'failure) => 'failure)

;; ===== 6. ellipsis 转义（R6RS (... ...)）=====
;; (... ...) 产一个字面 `...' datum（quote 模板），程序模板里也作为
;; 值保留（展开器 datum 嵌 syntax 保留）。
(define-syntax m-esc
  (syntax-rules ()
    ((_ x) '(... ...))))
(check (syntax->datum (m-esc 1)) => '...)
(define-syntax m-esc-p
  (syntax-rules ()
    ((_ x) (list x (... ...)))))
(check (syntax->datum (m-esc-p 1)) => '(1 ...))

;; ===== 7. 字面 `...' 转义 =====
;; 自定义 ellipsis 规则里字面写 `...' 会被转义（R6RS (... ...) 形式），
;; 匹配时按字面处理、不当作 ellipsis（Guile 同样如此）—— 所以
;; `(m 1 2 3)` 匹配不上 `(_ x ...)`（展开时报 no matching clause）。
(check (catch #t
         (lambda ()
           (expand-eval (quote (let-syntax ((m (syntax-rules :: ()
                                                 ((_ x ...) (list x ...)))))
                                  (m 1 2 3)))))
         (lambda args (quote no-match)))
       => 'no-match)

(check-report)
