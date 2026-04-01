;; (liii bag) 模块函数分类索引
;;
;; bag 是一种 multiset，和 set 不同，它会保留元素出现的次数。
;; 适合频次统计、去重前聚合，以及“允许重复值”的集合运算。

;; ==== 常见用法示例 ====
(import (liii bag))

;; 示例1：创建一个会保留重复元素的 bag
(define scores (bag 1 2 2 3))
(bag-size scores) ; => 4

;; 示例2：查询某个元素是否存在，以及它的代表值
(bag-member scores 2 #f) ; => 2

;; 示例3：向 bag 中加入新元素
(bag->list (bag-adjoin scores 5)) ; => 一个包含 1 2 2 3 5 的列表

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/bag "bag"
;;   bin/gf doc liii/bag "bag-union"

;; ==== 函数分类索引 ====

;; 一、构造函数
;; 用于创建和复制 bag 的函数
;;   bag                - 从多个元素直接创建 bag
;;   bag-unfold         - 通过 unfold 规则生成 bag
;;   bag->list          - 将 bag 转换成列表
;;   list->bag          - 将列表转换成 bag
;;   list->bag!         - 原地式构建 bag
;;   bag-copy           - 复制一个 bag
;;   bag-comparator     - 获取 bag 使用的比较器

;; 二、谓词函数
;; 用于判断 bag 状态和成员关系的函数
;;   bag?               - 判断对象是否为 bag
;;   bag-contains?      - 判断 bag 是否包含某个元素
;;   bag-empty?         - 判断 bag 是否为空
;;   bag-disjoint?      - 判断两个 bag 是否不相交

;; 三、统计与比较
;; 用于统计元素和比较 bag 的函数
;;   bag-size           - 获取 bag 中元素总数
;;   bag-find           - 查找满足条件的元素
;;   bag-count          - 统计满足条件的元素个数
;;   bag-any?           - 判断是否存在满足条件的元素
;;   bag-every?         - 判断是否所有元素都满足条件
;;   bag=?              - 判断两个 bag 是否相等
;;   bag<?              - 判断一个 bag 是否真子集于另一个
;;   bag>?              - 判断一个 bag 是否真超集于另一个
;;   bag<=?             - 判断一个 bag 是否为另一个的子集
;;   bag>=?             - 判断一个 bag 是否为另一个的超集

;; 四、集合运算
;; 用于执行 bag 之间集合代数的函数
;;   bag-union          - 计算并集
;;   bag-intersection   - 计算交集
;;   bag-difference     - 计算差集
;;   bag-xor            - 计算对称差
;;   bag-union!         - 原地计算并集
;;   bag-intersection!  - 原地计算交集
;;   bag-difference!    - 原地计算差集
;;   bag-xor!           - 原地计算对称差

;; 五、更新函数
;; 用于增删替换元素的函数
;;   bag-adjoin         - 向 bag 中加入元素
;;   bag-adjoin!        - 原地加入元素
;;   bag-replace        - 替换一个元素
;;   bag-replace!       - 原地替换一个元素
;;   bag-delete         - 删除一个元素
;;   bag-delete!        - 原地删除一个元素
;;   bag-delete-all     - 删除所有匹配元素
;;   bag-delete-all!    - 原地删除所有匹配元素
;;   bag-search!        - 通过搜索过程原地更新元素
