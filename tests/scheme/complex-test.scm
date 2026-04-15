;; (scheme complex) 模块函数分类索引
;;
;; complex 提供复数构造、实部虚部访问、模长和辐角计算等能力。
;; 它适合信号处理、几何计算和需要极坐标表示的数值场景。
;; ==== 常见用法示例 ====
(import (scheme complex))
;; 示例1：构造一个直角坐标形式的复数
(define z (make-rectangular 3 4))
(real-part z)
(imag-part z)
;; 示例2：读取复数的模长
(magnitude z)
;; 示例3：使用极坐标构造复数
(real-part (make-polar 2 0))
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc scheme/complex "make-rectangular"
;;   bin/gf doc scheme/complex "magnitude"
;; ==== 函数分类索引 ====
;; 一、构造函数
;; 用于创建复数的函数
;;   make-rectangular   - 通过实部和虚部创建复数
;;   make-polar         - 通过模长和辐角创建复数
;; 二、分量访问
;; 用于读取复数分量的函数
;;   real-part          - 获取实部
;;   imag-part          - 获取虚部
;; 三、极坐标属性
;; 用于读取复数模长和角度的函数
;;   magnitude          - 获取复数的模
;;   angle              - 获取复数的辐角
