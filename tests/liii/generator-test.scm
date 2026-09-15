;; (liii generator) 模块函数分类索引
;;
;; generator（生成器）是一个无参过程，每次调用返回下一个元素，耗尽时返回 eof-object。
;; accumulator（累加器）是一个单参过程，接收元素进行累计，接收 eof-object 时返回最终计算结果。
;; 本模块兼容 SRFI-158。

;; ==== 常见用法示例 ====
(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; 示例1：构造生成器并遍历
(define g (generator 1 2 3))
(check (g) => 1)
(check (g) => 2)
(check (g) => 3)
(check-true (eof-object? (g)))

;; 示例2：生成器转换为列表
(check (generator->list (make-iota-generator 4 10 2)) => '(10 12 14 16))

;; 示例3：生成器映射与过滤
(define g2 (gtake (gfilter odd? (make-range-generator 0 100)) 3))
(check (generator->list g2) => '(1 3 5))

;; 示例4：累加器收集
(define acc (list-accumulator))
(acc 'a)
(acc 'b)
(check (acc (eof-object)) => '(a b))

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/generator "generator"
;;   bin/gf doc liii/generator "gfilter"
;;   bin/gf doc liii/generator "make-accumulator"

;; ==== 函数分类索引 ====

;; 一、生成器构造函数
;;   generator                - 创建依次产出指定参数的生成器
;;   circular-generator       - 创建无限循环产出参数序列的生成器
;;   make-iota-generator      - 创建等差数列生成器
;;   make-range-generator     - 创建数值区间生成器
;;   make-coroutine-generator - 创建协程生成器
;;   make-for-each-generator  - 将 for-each 类过程转换为生成器
;;   make-unfold-generator    - 创建展开算法生成器

;; 二、转换为生成器
;;   list->generator          - 列表转生成器
;;   vector->generator        - 向量转生成器
;;   reverse-vector->generator - 逆序向量转生成器
;;   string->generator        - 字符串转生成器
;;   bytevector->generator    - 字节向量转生成器

;; 三、从生成器转换
;;   generator->list          - 生成器转列表
;;   generator->reverse-list  - 生成器转逆序列表
;;   generator->vector        - 生成器转向量
;;   generator->vector!       - 生成器写入现有向量
;;   generator->string        - 生成器转字符串
;;   generator-map->list      - 映射并转为列表

;; 四、生成器组合与变换
;;   gcons*                   - 前置插入元素
;;   gappend                  - 拼接多个生成器
;;   gflatten                 - 扁平化列表生成器
;;   ggroup                   - 分组成子列表
;;   gmerge                   - 有序归并多个生成器
;;   gmap                     - 映射生成器
;;   gcombine                 - 带状态结合生成器
;;   gfilter                  - 过滤保留满足谓词的元素
;;   gremove                  - 过滤移除满足谓词的元素
;;   gstate-filter            - 带状态过滤
;;   gtake                    - 取前 k 个元素
;;   gdrop                    - 跳过前 k 个元素
;;   gtake-while              - 满足条件时持续取元素
;;   gdrop-while              - 满足条件时持续跳过元素
;;   gdelete                  - 删除指定值
;;   gdelete-neighbor-dups    - 删除相邻重复元素
;;   gindex                   - 按索引生成器选取元素
;;   gselect                  - 按布尔生成器选取元素

;; 五、折叠与查找
;;   generator-fold           - 左折叠
;;   generator-for-each       - 遍历副作用
;;   generator-find           - 查找满足谓词的首个元素
;;   generator-count          - 统计满足谓词的元素数量
;;   generator-any            - 是否存在满足谓词的元素
;;   generator-every          - 是否所有元素满足谓词
;;   generator-unfold         - 使用展开器消费生成器

;; 六、累加器
;;   make-accumulator         - 创建自定义累加器
;;   list-accumulator         - 收集为列表
;;   reverse-list-accumulator - 收集为逆序列表
;;   vector-accumulator       - 收集为向量
;;   reverse-vector-accumulator - 收集为逆序向量
;;   vector-accumulator!      - 原地填充向量
;;   string-accumulator       - 收集为字符串
;;   bytevector-accumulator   - 收集为字节向量
;;   bytevector-accumulator!  - 原地填充字节向量
;;   sum-accumulator          - 求和累加器
;;   product-accumulator      - 乘积累加器

(check-report)
