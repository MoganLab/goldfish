;; (liii match) 模块测试文件
;; 原 (liii case) 库的 case* 已由 (liii match)（Alex Shinn 的可移植
;; 模式匹配器，Public Domain）替代：match 是 case* 的超集，case* 的
;; 字面量集合、列表、变量、谓词描述符均可用 match 的 or/字面量/
;; 变量/(? pred) 模式表达。
;;
;; match 是 expander 的 syntax-rules 宏，测试须经 expander 编译
;; （compile-program），故本文件像 expander-test 一样先加载 expander。

(load "liii/boot.scm")
(load "liii/reader.scm")
(load-source-file "expander/kernel-combined.scm")
(load-source-file "expander/lib/install.scm")
(install-standard-library!)

(define (run prog)
  (eval (compile-program (cons '(import (liii match)) prog)) (rootlet)))

(import (liii check))
(check-set-mode! 'report)

(check (run '((match 'yes ((or 'yes 'no) 'boolean) (_ 'unknown)))) => 'boolean)
(check (run '((match 'no ((or 'yes 'no) 'boolean) (_ 'unknown)))) => 'boolean)
(check (run '((match 'maybe ((or 'yes 'no) 'boolean) (_ 'unknown)))) => 'unknown)
(check (run '((match 42 ((or 1 2 3) 'small) ((or 42 100) 'big) (_ 'other)))) => 'big)
(check (run '((match "hello" ((or "hi" "hello") 'greeting) (_ 'other)))) => 'greeting)
(check (run '((match 3.14 ((or 1 2 3) 'integer) ((or 3.14 2.71) 'float) (_ 'other))))
  =>
  'float
) ;check
(check (run '((match '(1 2 3) ((1 2 3) 'matched) (_ 'not-matched)))) => 'matched)
(check (run '((match '(1 2 3) ((1 2) 'two) ((1 2 3) 'three) (_ 'other))))
  =>
  'three
) ;check
(check (run '((match '(1 2) ((x y) (+ x y)) (_ 0)))) => 3)
(check (run '((match '(hello world) ((first second) (list second first)) (_ '()))))
  =>
  '(world hello)
) ;check
(check (run '((match '(5 5) ((x y) (if (= x y) 'same 'different)) (_ 'unknown))))
  =>
  'same
) ;check
(check (run '((match 42 ((? integer?) 'integer) ((? string?) 'string) (_ 'other))))
  =>
  'integer
) ;check
(check (run '((match "hello" ((? integer?) 'integer) ((? string?) 'string) (_ 'other))))
  =>
  'string
) ;check
(check (run '((match 42 ((? integer? x) (* x 2)) (_ 0)))) => 84)

;; =============================================================================
;; 高级模式（原 case* 测试的完整覆盖）：
;; - 字面量符号（(quote +)）
;; - ellipsis 收集（(op args ...)）
;; - 嵌套结构 + 谓词守卫
;; - 裸值 + 谓词
;; =============================================================================

;; calc：+/- 为字面量，其余 op 收集 args，裸整数直接返回
(check (run '((match '(+ 3 4)
                (((quote +) a b) (+ a b))
                (((quote -) a b) (- a b))
                ((op args ...) (list 'unhandled-op op args))
                ((? integer? x) x)
                (_ 'invalid))))
  => 7)
(check (run '((match '(- 10 3)
                (((quote +) a b) (+ a b))
                (((quote -) a b) (- a b))
                ((op args ...) (list 'unhandled-op op args))
                ((? integer? x) x)
                (_ 'invalid))))
  => 7)
(check (run '((match '(* 2 3)
                (((quote +) a b) (+ a b))
                (((quote -) a b) (- a b))
                ((op args ...) (list 'unhandled-op op args))
                ((? integer? x) x)
                (_ 'invalid))))
  => '(unhandled-op * (2 3)))
(check (run '((match 42
                (((quote +) a b) (+ a b))
                (((quote -) a b) (- a b))
                ((op args ...) (list 'unhandled-op op args))
                ((? integer? x) x)
                (_ 'invalid))))
  => 42)

;; 嵌套结构 + 谓词守卫
(check (run '((match '(user (name "Alice") (age 30))
                (((quote user) (name (? string? n)) (age (? integer? a))) (and (> a 0) (< a 150)))
                (_ #f))))
  => #t)
(check (run '((match '(user (name "Bob") (age 200))
                (((quote user) (name (? string? n)) (age (? integer? a))) (and (> a 0) (< a 150)))
                (_ #f))))
  => #f)
(check (run '((match '(user (name 123) (age 30))
                (((quote user) (name (? string? n)) (age (? integer? a))) (and (> a 0) (< a 150)))
                (_ #f))))
  => #f)
(check (run '((match '(other (name "X") (age 20))
                (((quote user) (name (? string? n)) (age (? integer? a))) (and (> a 0) (< a 150)))
                (_ #f))))
  => #f)

;; binop：三个元素
(check (run '((match '(+ 1 2) ((op left right) (list 'binop op left right)) (_ #f))))
  => '(binop + 1 2))
(check (run '((match '(* x y) ((op left right) (list 'binop op left right)) (_ #f))))
  => '(binop * x y))
(check (run '((match '(+ 1 2 3) ((op left right) (list 'binop op left right)) (_ #f))))
  => #f)
(check (run '((match 42 ((op left right) (list 'binop op left right)) (_ #f))))
  => #f)

;; if 字面量
(check (run '((match '(if (> x 0) x (- x))
                (((quote if) c t e) (list 'if-expr c t e))
                (_ #f))))
  => '(if-expr (> x 0) x (- x)))
(check (run '((match '(if flag then)
                (((quote if) c t e) (list 'if-expr c t e))
                (_ #f))))
  => #f)
