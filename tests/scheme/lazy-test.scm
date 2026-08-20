;; (scheme lazy) 模块文档与测试
;;
;; `(scheme lazy)` 提供延迟求值（lazy evaluation）相关的过程与语法：
;; 惰性 promise 的创建与强制求值。
;;
;; ==== 语法与过程 ====
;;
;;   (delay expr)                创建惰性 promise，首次 force 时求值
;;   (delay-force expr)          创建惰性 promise，其值本身是一个 promise
;;   (force promise)             强制求值 promise；已求值则直接返回缓存值
;;   (make-promise obj)          将 obj 包装为 promise（若已是 promise 则原样返回）
;;   (promise? obj)              判断 obj 是否为 promise
;;
;; ==== 说明 ====
;;
;; 1. (delay expr) 创建的 promise 只求值一次，后续 force 返回缓存值
;; 2. R7RS 允许 force 在非惰性实现中等价于直接求值，但 Goldfish 提供真正的惰性语义
;; 3. make-promise 用于将普通对象包装成 promise，以便统一处理
;;
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc scheme/lazy "delay"
;;   bin/gf doc scheme/lazy "force"
(import (liii check) (scheme lazy))
(check-set-mode! 'report-failed)

;; ==== 测试：delay / force 基本用法 ====

(define lazy-x (delay (+ 1 2)))
(check (promise? lazy-x) => #t)
(check (force lazy-x) => 3)
;; 已求值的 promise 再次 force 返回缓存值
(check (force lazy-x) => 3)

;; ==== 测试：delay 只求值一次 ====

(define evaluated 0)

(define lazy-once (delay (begin (set! evaluated (+ evaluated 1)) 42)))
(check evaluated => 0)
(check (force lazy-once) => 42)
(check evaluated => 1)
(check (force lazy-once) => 42)
(check evaluated => 1)

;; ==== 测试：delay-force ====

(define lazy-df (delay-force (delay 7)))
(check (force lazy-df) => 7)

;; ==== 测试：make-promise ====
(check (promise? (make-promise 5)) => #t)
(check (force (make-promise 5)) => 5)
;; make-promise 对已有 promise 原样返回

(define p (delay 1))
(check (eq? (make-promise p) p) => #t)
;; make-promise 对非 promise 包装
(check (eq? (make-promise 5) 5) => #f)

;; ==== 测试：promise? ====
(check (promise? (delay 1)) => #t)
(check (promise? (make-promise 'x)) => #t)
(check (promise? 5) => #f)
(check (promise? '()) => #f)
(check (promise? (list 1 2)) => #f)

(check-report)
