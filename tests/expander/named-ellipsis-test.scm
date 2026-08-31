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
;; parse-template 已识别 (... ...) 产字面 `...' 节点；但字面 `...' 作为
;; syntax 对象嵌在 datum 里，goldfish 展开器会递归展开它（当程序/值），
;; 而非 Racket 那样保留 datum 为值。结果 quote 模板得到 `(...)'（列表）
;; 而非 `...'，程序模板报 unbound。与 quasisyntax 的 `(syntax X)' 子模板
;; 同一展开器限制（需支持 datum 嵌 syntax）。
(define-syntax m-esc
  (syntax-rules ()
    ((_ x) '(... ...))))
(check (pair? (m-esc 1)) => #t)

;; ===== 7. 已知限制 =====
;; 自定义 ellipsis 时，规则里字面写的 `...' 不会被转义（Guile 会），
;; 也会被当作 ellipsis。记录为限制。

(check-report)
