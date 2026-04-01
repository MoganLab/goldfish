;; (liii timeit) 模块函数分类索引
;;
;; (liii timeit) 提供性能测试工具，用于测量代码执行时间。
;; 该库适合用于代码优化、性能基准测试和算法比较。

;; ==== 常见用法示例 ====
(import (liii timeit))

;; 示例1：测量简单运算的执行时间
(timeit (lambda () (+ 1 2))
        (lambda () #t)
        1000000
) ;timeit

;; 示例2：使用 setup 函数初始化状态
(let ((lst '()))
  (timeit (lambda () (set! lst (cons 1 lst)))
          (lambda () (set! lst '()))
          10000
  ) ;timeit
) ;let

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/timeit "function-name"

;; ==== 函数分类索引 ====
;;
;; 一、性能测试
;;   timeit       - 测量代码执行时间
