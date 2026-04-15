;; (scheme eval) 模块函数分类索引
;;
;; eval 提供动态构造求值环境和在指定环境中执行表达式的能力。
;; 它适合 REPL、脚本引擎、DSL 和需要增量求值的运行时场景。
;; ==== 常见用法示例 ====
(import (scheme eval))
;; 示例1：创建一个只导入 `(scheme base)` 的求值环境
(define env
  (environment '(scheme base))
) ;define
;; 示例2：在指定环境中对表达式求值
(eval '(+ 1 2) env)
;; 示例3：使用 import set 变换定制可见名字
(eval '(base-square 4)
  (environment '(prefix (scheme base) base-)
  ) ;environment
) ;eval
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc scheme/eval "environment"
;;   bin/gf doc scheme/eval "eval"
;; ==== 函数分类索引 ====
;; 一、环境构造函数
;; 用于创建可供动态求值使用环境的函数
;;   environment        - 根据 import set 构造新的求值环境
;; 二、表达式求值函数
;; 用于在指定环境中执行表达式的函数
;;   eval               - 在给定环境中对表达式求值
