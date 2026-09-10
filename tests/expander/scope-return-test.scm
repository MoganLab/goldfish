(import (liii check) (goldfish))

;; use-scope 返回语义锁定（对应 kernel/context.scm context-return）：
;; 子表达式展开后，调用方的解析上下文必须恢复（scopes 不泄漏），只有
;; 宏输出重展开 / local-expand 等显式延续点才带走结果 scopes。
;; 若有人把 context-return 改成无条件合并（union），1 会捕获成 inner；
;; 若有人把延续点改成丢弃，2 的内部宏引用会解析失败。

;; ===== 1. 宏引入绑定不捕获 use-site 同名（shadowing 隔离）=====
(let ((x 'outer))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (let ((x 'inner))
      (check (m) => 'outer))))

;; ===== 2. 宏输出里的宏调用能解析（重展开延续 scopes）=====
(define-syntax inner-helper
  (syntax-rules () ((inner-helper x) (+ x 1))))
(define-syntax outer-user
  (syntax-rules () ((outer-user e) (inner-helper e))))
(check (outer-user 41) => 42)

;; ===== 3. 内部 define 经宏生成：定义在 body 可见，RHS 只求值一次 =====
(let ((z 10))
  (define-syntax def-two
    (syntax-rules ()
      ((def-two a b e) (begin (define a e) (define b (+ e 1))))))
  (def-two p q z)
  (check (list p q) => '(10 11)))

(check-report)
