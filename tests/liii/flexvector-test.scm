;; (liii flexvector) 模块函数分类索引
;;
;; flexvector 是可变长向量，兼具向量随机访问能力和动态扩缩容能力。
;; 适合需要频繁追加、删除、映射和分段处理的顺序容器场景。

;; ==== 常见用法示例 ====
(import (liii flexvector))

;; 示例1：创建一个可变长向量并访问元素
(define fv (flexvector 1 2 3))
(flexvector-length fv) ; => 3
(flexvector-ref fv 1) ; => 2

;; 示例2：向 flexvector 末尾追加元素
(flexvector->list (flexvector-add-back! (flexvector-copy fv) 4)) ; => (1 2 3 4)

;; 示例3：拼接两个 flexvector
(flexvector->list (flexvector-append fv (flexvector 4 5))) ; => (1 2 3 4 5)

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/flexvector "flexvector"
;;   bin/gf doc liii/flexvector "flexvector-append"

;; ==== 函数分类索引 ====

;; 一、构造函数
;; 用于创建和基本访问 flexvector 的函数
;;   make-flexvector             - 创建指定长度的 flexvector
;;   flexvector                  - 从多个元素直接创建 flexvector
;;   flexvector?                 - 判断对象是否为 flexvector
;;   flexvector-empty?           - 判断 flexvector 是否为空
;;   flexvector-ref              - 按索引访问元素
;;   flexvector-front            - 获取首元素
;;   flexvector-back             - 获取尾元素
;;   flexvector-length           - 获取长度

;; 二、原地修改
;; 用于原地增删改元素的函数
;;   flexvector-set!             - 原地设置元素
;;   flexvector-add!             - 在指定位置插入元素
;;   flexvector-add-back!        - 在尾部追加元素
;;   flexvector-add-front!       - 在头部插入元素
;;   flexvector-remove!          - 删除指定位置元素
;;   flexvector-remove-back!     - 删除尾元素
;;   flexvector-remove-front!    - 删除首元素
;;   flexvector-remove-range!    - 删除一段区间
;;   flexvector-clear!           - 清空所有元素
;;   flexvector-fill!            - 用同一值填充区间
;;   flexvector-swap!            - 交换两个位置的元素
;;   flexvector-reverse!         - 原地反转

;; 三、复制与类型转换
;; 用于复制 flexvector 或在不同容器间转换的函数
;;   flexvector-copy             - 复制 flexvector
;;   flexvector-copy!            - 把内容复制到另一 flexvector
;;   flexvector-reverse-copy     - 生成反向副本
;;   flexvector-reverse-copy!    - 原地复制反向内容
;;   flexvector->vector          - 转为普通向量
;;   vector->flexvector          - 向量转为 flexvector
;;   flexvector->list            - 转为列表
;;   list->flexvector            - 列表转为 flexvector
;;   reverse-flexvector->list    - 反向转为列表
;;   reverse-list->flexvector    - 反向列表转为 flexvector
;;   flexvector->string          - 转为字符串
;;   string->flexvector          - 字符串转为 flexvector

;; 四、遍历与搜索
;; 用于遍历、统计和查找 flexvector 的函数
;;   flexvector-for-each         - 遍历元素
;;   flexvector-for-each/index   - 带索引遍历元素
;;   flexvector-map              - 映射生成新 flexvector
;;   flexvector-map!             - 原地映射
;;   flexvector-map/index        - 带索引映射
;;   flexvector-map/index!       - 原地带索引映射
;;   flexvector-fold             - 左折叠
;;   flexvector-fold-right       - 右折叠
;;   flexvector-filter           - 过滤元素
;;   flexvector-filter!          - 原地过滤元素
;;   flexvector-filter/index     - 带索引过滤
;;   flexvector-filter/index!    - 原地带索引过滤
;;   flexvector-append-map       - 映射后拼接
;;   flexvector-append-map/index - 带索引映射后拼接
;;   flexvector-count            - 统计满足条件的元素个数
;;   flexvector-cumulate         - 计算累计结果
;;   flexvector-index            - 查找第一个满足条件的位置
;;   flexvector-index-right      - 从右侧查找位置
;;   flexvector-skip             - 跳过满足条件的前缀
;;   flexvector-skip-right       - 从右侧跳过满足条件的后缀
;;   flexvector-any              - 判断是否存在满足条件的元素
;;   flexvector-every            - 判断是否全部满足条件
;;   flexvector-binary-search    - 执行二分搜索
;;   flexvector-partition        - 拆分为满足与不满足两部分

;; 五、拼接与展开
;; 用于组合 flexvector 或通过生成器构造 flexvector 的函数
;;   flexvector-append           - 拼接多个 flexvector
;;   flexvector-concatenate      - 连接一个 flexvector 列表
;;   flexvector-append-subvectors - 拼接多个子向量区间
;;   flexvector-append!          - 原地拼接
;;   flexvector=?                - 判断两个 flexvector 是否相等
;;   flexvector-unfold           - 通过 unfold 构造 flexvector
;;   flexvector-unfold-right     - 从右侧 unfold 构造 flexvector
;;   flexvector->generator       - 转为生成器
;;   generator->flexvector       - 生成器转为 flexvector
