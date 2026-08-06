;; (scheme let) 模块函数分类索引
;;
;; `(scheme let)` 提供 S7 风格的环境（let）操作函数。
;; 在 S7/Goldfish 中，环境本身是一等公民对象，可以创建、查询、组合和修改，
;; 适合实现模块系统、沙箱求值、面向对象等需要显式操控环境的场景。
;;
;; ==== 常见用法示例 ====
(import (scheme let))
;; 示例1：创建一个独立环境并访问其中的绑定

(define e (inlet 'x 1 'y 2))
(let-ref e 'x)
;; 示例2：基于现有环境派生新环境（不修改原环境）

(define e2 (sublet e 'z 3))
(let-ref e2 'z)
;; 示例3：在指定环境中求值
(eval '(+ x y) e)
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc scheme/let "inlet"
;;   bin/gf doc scheme/let "let-ref"
;; ==== 函数分类索引 ====
;; 一、谓词
;; 用于判断对象类型的函数
;;   let?               - 判断对象是否为 let（环境对象）
;;   openlet?           - 判断 let 是否为开放环境
;;   funclet?           - 判断 let 是否为函数的闭包环境
;; 二、环境获取
;; 用于获取已有环境的函数
;;   curlet             - 返回当前词法环境
;;   outlet             - 返回环境的外层环境
;;   rootlet            - 返回根环境
;;   owlet              - 返回全局（最外层）环境链
;;   funclet            - 返回函数的闭包环境
;; 三、环境构造与操作
;; 用于创建或修改环境的函数
;;   inlet              - 创建新的环境
;;   sublet             - 基于现有环境派生新环境（不改父环境）
;;   varlet             - 向环境中添加绑定（就地修改）
;;   cutlet             - 从环境中删除绑定（就地修改）
;;   openlet            - 将环境标记为开放环境
;;   coverlet           - 将开放环境恢复为封闭环境
;;   unlet              - 返回内建函数原始绑定所在的环境
;; 四、绑定访问
;; 用于读写环境中绑定的函数
;;   let-ref            - 读取环境中符号对应的值
;;   let-set!           - 修改环境中符号对应的值
;;   let->list          - 将环境的绑定转换为关联列表
;; 五、符号查找
;; 用于在环境中查找符号绑定的函数
;;   symbol->value      - 在指定环境中查找符号的值
;;   symbol->dynamic-value - 在动态环境中查找符号的值
