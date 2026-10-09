;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

;;; ====================================================================
;;; Scheme in Scheme: 30 行元循环求值器 (Meta-circular Evaluator)
;;;
;;; 参考书目: "A Scheme Primer" (by Christine Lemmer-Webber)
;;; 对应章节: 第 12 章 "Scheme in Scheme"
;;;
;;; 本示例使用 Goldfish Scheme 的模式匹配库 (liii match) 实现第12章代码。
;;;
;;; 说明:
;;; 1. 原书使用 Guile 的 (ice-9 match)，Goldfish Scheme 使用 (liii match)。
;;; 2. 原书中变参 lambda 写为 `(lambda (. vals) ...)`，在标准 Scheme 与
;;;    Goldfish (S7) 中使用变长参数形式 `(lambda vals ...)`。
;;; 3. 在 S7 Scheme 中，字面量引用 `'expr` 展开后首项为内置 syntax `#_quote`，
;;;    本实现兼顾常规符号 `'quote` 与 S7 内置引用语法 `#_quote`。
;;; ====================================================================

(import (scheme base) (scheme write) (liii match))

;; 判断是否为引用关键字（兼容标准符号 'quote 与 S7 reader 的内置 quote syntax）

(define (quote-id? x)
  (or (eq? x 'quote) (and (syntax? x) (eq? x (car ''_))))
) ;define

;; 环境查找：在关联列表 env 中查找变量名 name

(define (env-lookup env name)
  (match (assoc name env) ((_key . val) val) (_ (error "Variable unbound:" name)))
) ;define

;; 环境扩展：将一组变量名 names 与对应的值 vals 绑定并加入环境

(define (extend-env env names vals)
  (if (eq? names '())
    env
    (cons (cons (car names) (car vals)) (extend-env env (cdr names) (cdr vals)))
  ) ;if
) ;define

;; 核心求值器：约 30 行的 Scheme in Scheme 求值器

(define (evaluate expr env)
  (match expr
    ;; 支持内置基础类型 (布尔值、数值)
    ((or #t #f (? number?)) expr)

    ;; 引用 (Quoting)
    ((or ('quote quoted-expr) ((? quote-id?) quoted-expr)) quoted-expr)

    ;; 变量查找 (Variable lookup)
    ((? symbol? name) (env-lookup env name))

    ;; 条件分支 (Conditionals)
    (('if test consequent alternate)
     (if (evaluate test env) (evaluate consequent env) (evaluate alternate env))
    ) ;

    ;; 过程定义 (Lambdas)
    (('lambda (args ...) body)
     (lambda vals (evaluate body (extend-env env args vals)))
    ) ;

    ;; 过程调用 (Procedure Invocation / Application)
    ((proc-expr arg-exprs ...)
     (apply (evaluate proc-expr env)
       (map (lambda (arg-expr) (evaluate arg-expr env)) arg-exprs)
     ) ;apply
    ) ;
  ) ;match
) ;define

;; ====================================================================
;; 原书各小节示例验证与运行输出
;; ====================================================================

(display "========================================================\n")
(display "  A Scheme Primer - 第 12 章: Scheme in Scheme\n")
(display "  基于 Goldfish Scheme (liii match) 实现\n")
(display "========================================================\n\n")

;; 1. 环境查找与环境扩展
(display "1. 环境操作 (env-lookup & extend-env):\n")

(define test-env '((foo . newer-foo) (bar . bar) (foo . older-foo)))
(display "   在环境中查找 'foo (后绑定的会遮蔽先绑定的):\n")
(display "   => ")
(write (env-lookup test-env 'foo))
(newline)

(display "   扩展环境 (extend-env):\n")
(display "   => ")
(write (extend-env '((foo . foo-val)) '(bar quux) '(bar-val quux-val)))
(newline)
(newline)

;; 2. 基础原子类型与引用求值
(display "2. 基础求值 (字面量、Quote 与变量查找):\n")
(display "   (evaluate #t '())              => ")
(write (evaluate #t '()))
(newline)

(display "   (evaluate #f '())              => ")
(write (evaluate #f '()))
(newline)

(display "   (evaluate 33 '())              => ")
(write (evaluate 33 '()))
(newline)

(display "   (evaluate -2/3 '())            => ")
(write (evaluate -2/3 '()))
(newline)

(display "   (evaluate ''foo '())           => ")
(write (evaluate ''foo '()))
(newline)

(display "   (evaluate ''(1 2 3) '())       => ")
(write (evaluate ''(1 2 3) '()))
(newline)

(display "   (evaluate 'x '((x . 33)))      => ")
(write (evaluate 'x '((x . 33))))
(newline)

(display "   (evaluate '((lambda (x) x) 33) '())\n")
(display "                                  => ")
(write
  (evaluate '((lambda (x) x) 33) '())
) ;write
(newline)
(newline)

;; 3. 过程构建与高阶过程调用
(display "3. 过程构建 (Lambda):\n")
(display "   ((evaluate '(lambda (x y) x) '()) 'first 'second)\n")
(display "                                  => ")
(write
 ((evaluate '(lambda (x y) x) '()) 'first 'second)
) ;write
(newline)

(display "   ((evaluate '(lambda (x y) y) '()) 'first 'second)\n")
(display "                                  => ")
(write
 ((evaluate '(lambda (x y) y) '()) 'first 'second)
) ;write
(newline)
(newline)

;; 4. 算术环境 math-env
(display "4. 算术求值 (math-env):\n")

(define math-env `((+ . ,+) (- . ,-) (* . ,*) (/ . ,/)))

(display "   (evaluate '(* (- 8 (/ 30 5)) 21) math-env)\n")
(display "                                  => ")
(write
  (evaluate '(* (- 8 (/ 30 5)) 21) math-env)
) ;write
(newline)

(display "   (evaluate '((lambda (x) (* x x)) 4) math-env)\n")
(display "                                  => ")
(write
  (evaluate '((lambda (x) (* x x)) 4) math-env)
) ;write
(newline)
(newline)

;; 5. 自传递 Boot 技巧计算斐波那契数列 (Fibonacci)
(display "5. 高级计算: 纯 Scheme 解释器中计算斐波那契数列 (fib 10):\n"
) ;display

(define fib-program
  '((lambda (prog arg) (prog prog arg))
    (lambda (fib n)
      (if (= n 0) 0 (if (= n 1) 1 (+ (fib fib (+ n -1)) (fib fib (+ n -2))))))
    10)
) ;define

(define fib-env `((+ . ,+) (= . ,=)))

(display "   (evaluate fib-program fib-env)\n")
(display "                                  => ")
(write (evaluate fib-program fib-env))
(newline)
(display "\n=== 执行完成 ===\n")
