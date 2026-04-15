;; (liii flexvector) 模块函数分类索引
;;
;; flexvector 是可变长向量，兼具向量随机访问能力和动态扩缩容能力。
;; 适合需要频繁追加、删除、映射和分段处理的顺序容器场景。


;; ==== 常见用法示例 ====
(import (liii flexvector))


;; 示例1：创建一个可变长向量并访问元素
(define fv (flexvector 1 2 3))
(flexvector-length fv)
(flexvector-ref fv 1)


;; 示例2：向 flexvector 末尾追加元素
(flexvector->list (flexvector-add-back! (flexvector-copy fv)
                    4
                  ) ;flexvector-add-back!
) ;flexvector->list


;; 示例3：拼接两个 flexvector
(flexvector->list (flexvector-append fv (flexvector 4 5))
) ;flexvector->list


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/flexvector "flexvector"
;;   bin/gf doc liii/flexvector "flexvector-append"


;; ==== 复杂度速查表 ====
;;
;; O(1) 操作（常数时间）:
;;   flexvector?, flexvector-empty?, flexvector-length
;;   flexvector-ref, flexvector-front, flexvector-back
;;   flexvector-set!, flexvector-add-back!, flexvector-remove-back!
;;   flexvector-clear!, flexvector-swap!
;;
;; O(n) 操作（线性时间）:
;;   flexvector-copy, flexvector->list, list->flexvector
;;   flexvector-map, flexvector-for-each, flexvector-fold, flexvector-filter
;;   flexvector-index, flexvector-count, flexvector-any, flexvector-every
;;   flexvector-add-front!, flexvector-remove!, flexvector-remove-front!
;;   flexvector-add!, flexvector-fill!, flexvector-reverse!
;;   flexvector-binary-search (O(log n) 但内部实现可能线性)
;;
;; ==== 函数分类索引 ====


;; 一、构造函数 (Constructors) - O(1) 或 O(n)
;; 用于创建和基本访问 flexvector 的函数
;;   make-flexvector             - O(n) 创建指定长度的 flexvector
;;   flexvector                  - O(n) 从多个元素直接创建 flexvector
;;   flexvector?                 - O(1) 判断对象是否为 flexvector
;;   flexvector-empty?           - O(1) 判断 flexvector 是否为空
;;   flexvector-ref              - O(1) 按索引访问元素
;;   flexvector-front            - O(1) 获取首元素
;;   flexvector-back             - O(1) 获取尾元素
;;   flexvector-length           - O(1) 获取长度


;; 二、原地修改 (Mutators) - O(1) 或 O(n)
;; 用于原地增删改元素的函数
;;   flexvector-set!             - O(1) 原地设置元素
;;   flexvector-add!             - O(n) 在指定位置插入元素
;;   flexvector-add-back!        - O(1)* 在尾部追加元素（均摊）
;;   flexvector-add-front!       - O(n) 在头部插入元素
;;   flexvector-remove!          - O(n) 删除指定位置元素
;;   flexvector-remove-back!     - O(1) 删除尾元素
;;   flexvector-remove-front!    - O(n) 删除首元素
;;   flexvector-remove-range!    - O(n) 删除一段区间
;;   flexvector-clear!           - O(1) 清空所有元素
;;   flexvector-fill!            - O(n) 用同一值填充区间
;;   flexvector-swap!            - O(1) 交换两个位置的元素
;;   flexvector-reverse!         - O(n) 原地反转


;; 三、复制与类型转换 (Conversion) - O(n)
;; 用于复制 flexvector 或在不同容器间转换的函数
;;   flexvector-copy             - O(n) 复制 flexvector
;;   flexvector-copy!            - O(n) 把内容复制到另一 flexvector
;;   flexvector-reverse-copy     - O(n) 生成反向副本
;;   flexvector-reverse-copy!    - O(n) 原地复制反向内容
;;   flexvector->vector          - O(n) 转为普通向量
;;   vector->flexvector          - O(n) 向量转为 flexvector
;;   flexvector->list            - O(n) 转为列表
;;   list->flexvector            - O(n) 列表转为 flexvector
;;   reverse-flexvector->list    - O(n) 反向转为列表
;;   reverse-list->flexvector    - O(n) 反向列表转为 flexvector
;;   flexvector->string          - O(n) 转为字符串
;;   string->flexvector          - O(n) 字符串转为 flexvector


;; 四、遍历与搜索 (Iteration & Search) - O(n) 或 O(log n)
;; 用于遍历、统计和查找 flexvector 的函数
;;   flexvector-for-each         - O(n) 遍历元素
;;   flexvector-for-each/index   - O(n) 带索引遍历元素
;;   flexvector-map              - O(n) 映射生成新 flexvector
;;   flexvector-map!             - O(n) 原地映射
;;   flexvector-map/index        - O(n) 带索引映射
;;   flexvector-map/index!       - O(n) 原地带索引映射
;;   flexvector-fold             - O(n) 左折叠
;;   flexvector-fold-right       - O(n) 右折叠
;;   flexvector-filter           - O(n) 过滤元素
;;   flexvector-filter!          - O(n) 原地过滤元素
;;   flexvector-filter/index     - O(n) 带索引过滤
;;   flexvector-filter/index!    - O(n) 原地带索引过滤
;;   flexvector-append-map       - O(n) 映射后拼接
;;   flexvector-append-map/index - O(n) 带索引映射后拼接
;;   flexvector-count            - O(n) 统计满足条件的元素个数
;;   flexvector-cumulate         - O(n) 计算累计结果
;;   flexvector-index            - O(n) 查找第一个满足条件的位置
;;   flexvector-index-right      - O(n) 从右侧查找位置
;;   flexvector-skip             - O(n) 跳过满足条件的前缀
;;   flexvector-skip-right       - O(n) 从右侧跳过满足条件的后缀
;;   flexvector-any              - O(n) 判断是否存在满足条件的元素
;;   flexvector-every            - O(n) 判断是否全部满足条件
;;   flexvector-binary-search    - O(log n) 执行二分搜索（需有序）
;;   flexvector-partition        - O(n) 拆分为满足与不满足两部分


;; 五、拼接与展开 (Concatenation & Unfolding) - O(n)
;; 用于组合 flexvector 或通过生成器构造 flexvector 的函数
;;   flexvector-append           - O(n) 拼接多个 flexvector
;;   flexvector-concatenate      - O(n) 连接一个 flexvector 列表
;;   flexvector-append-subvectors - O(n) 拼接多个子向量区间
;;   flexvector-append!          - O(n) 原地拼接
;;   flexvector=?                - O(n) 判断两个 flexvector 是否相等
;;   flexvector-unfold           - O(n) 通过 unfold 构造 flexvector
;;   flexvector-unfold-right     - O(n) 从右侧 unfold 构造 flexvector
;;   flexvector->generator       - O(1) 转为生成器
;;   generator->flexvector       - O(n) 生成器转为 flexvector


;; ==== AI 使用建议 ====
;;
;; 1. 需要频繁尾部操作时使用 flexvector，而非 list
;;    - flexvector-add-back! 是 O(1) 均摊
;;    - cons 到 list 头部是 O(1) 但顺序相反
;;
;; 2. 需要随机访问时使用 flexvector-ref (O(1))
;;    - list-ref 是 O(n)，不适合频繁随机访问
;;
;; 3. 前端操作较慢（O(n)），如有需要可考虑：
;;    - 使用双端队列结构
;;    - 或使用 reverse-list->flexvector 一次性构建
;;
;; 4. 遍历操作优先使用 for-each 而非 map 当不需要结果时
;;
;; 5. 大量元素时先使用 make-flexvector 预分配再填充，
;;    比逐个 add-back! 更高效
