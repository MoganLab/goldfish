;; (liii vector) 模块函数分类索引
;;
;; vector 提供向量构造、切片、查找、过滤和原地更新等常用操作。
;; 它适合随机访问密集序列，以及需要保留索引语义的批量数据处理。

;; ==== 常见用法示例 ====
(import (liii vector))

;; 示例1：截取向量前半段
(vector->list (vector-take #(1 2 3 4) 2)) ; => (1 2)

;; 示例2：判断向量中是否包含某个元素
(vector-contains? #(1 2 3) 2) ; => #t

;; 示例3：跳过前一个元素并转回列表
(vector->list (vector-drop #(1 2 3 4) 1)) ; => (2 3 4)

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/vector "vector-take"
;;   bin/gf doc liii/vector "vector-contains?"

;; ==== 函数分类索引 ====

;; 一、构造与访问
;; 用于创建和读取向量的函数
;;   make-vector         - 创建指定长度向量
;;   vector              - 从多个元素直接构造向量
;;   list->vector        - 列表转向量
;;   vector->list        - 向量转列表
;;   vector-length       - 获取向量长度
;;   vector-ref          - 按索引读取元素
;;   vector-set!         - 原地设置元素

;; 二、复制与原地修改
;; 用于复制和调整向量内容的函数
;;   vector-copy         - 复制向量
;;   vector-copy!        - 把内容复制到另一向量
;;   vector-fill!        - 用同一值填充区间
;;   vector-swap!        - 交换两个位置的元素
;;   vector-reverse!     - 原地反转向量

;; 三、切片与过滤
;; 用于截取和筛选向量元素的函数
;;   vector-take         - 取前 n 个元素
;;   vector-drop         - 跳过前 n 个元素
;;   vector-take-right   - 取后 n 个元素
;;   vector-drop-right   - 跳过后 n 个元素
;;   vector-filter       - 过滤元素

;; 四、搜索与统计
;; 用于查找、判断和统计向量内容的函数
;;   vector-empty?       - 判断向量是否为空
;;   vector-contains?    - 判断是否包含某个元素
;;   vector-index        - 查找第一个满足条件的位置
;;   vector-index-right  - 从右侧查找位置
;;   vector-skip         - 跳过前方满足条件的元素
;;   vector-skip-right   - 从右侧跳过满足条件的元素
;;   vector-count        - 统计满足条件的元素个数
;;   vector-any          - 判断是否存在满足条件的元素
;;   vector-every        - 判断是否全部满足条件
;;   vector-partition    - 拆分为两部分

;; 五、遍历与变换
;; 用于映射、折叠和类型转换的函数
;;   vector-map          - 映射元素
;;   vector-for-each     - 遍历元素
;;   vector-fold         - 左折叠
;;   vector-fold-right   - 右折叠
;;   vector-cumulate     - 计算累计结果
;;   vector-append       - 拼接多个向量
;;   vector->string      - 向量转字符串
;;   string->vector      - 字符串转向量
;;   reverse-list->vector - 反向列表转向量
