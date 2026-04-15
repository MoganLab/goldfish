;; (liii set) 模块函数分类索引
;;
;; set 提供去重后的集合结构，适合成员测试、去重、集合代数和无序数据聚合。
;; 与 bag 不同，set 不保留重复元素的计数。


;; ==== 常见用法示例 ====
(import (liii set))


;; 示例1：构造一个普通集合
(define s (set 1 2 3))
(set-contains? s 2)


;; 示例2：读取集合大小
(set-size s)


;; 示例3：计算两个集合的并集
(set->list (set-union s (set 3 4)))


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/set "set"
;;   bin/gf doc liii/set "set-union"


;; ==== 函数分类索引 ====


;; 一、构造函数
;; 用于创建和复制集合的函数
;;   set                     - 从多个元素直接构造集合
;;   set-unfold              - 通过 unfold 规则构造集合
;;   list->set               - 从列表构造集合
;;   list->set!              - 原地式从列表构造集合
;;   set-copy                - 复制集合
;;   set->list               - 转为列表
;;   list->set-with-comparator - 使用指定比较器从列表构造集合
;;   make-set-with-comparator - 创建带比较器的空集合


;; 二、谓词与访问
;; 用于判断集合状态和读取成员的函数
;;   set?                    - 判断对象是否为集合
;;   set-contains?           - 判断是否包含某个元素
;;   set-empty?              - 判断集合是否为空
;;   set-disjoint?           - 判断两个集合是否不相交
;;   set-element-comparator  - 获取元素比较器
;;   set-size                - 获取元素个数
;;   set=?                   - 判断两个集合是否相等
;;   set<?                   - 判断一个集合是否真子集于另一个
;;   set>?                   - 判断一个集合是否真超集于另一个
;;   set<=?                  - 判断一个集合是否为另一个的子集
;;   set>=?                  - 判断一个集合是否为另一个的超集
;;   set-any?                - 判断是否存在满足条件的元素
;;   set-every?              - 判断是否全部满足条件
;;   set-find                - 查找满足条件的元素
;;   set-count               - 统计满足条件的元素个数
;;   set-member              - 返回等值成员


;; 三、遍历与过滤
;; 用于遍历、映射和筛选集合的函数
;;   set-map                 - 映射集合元素
;;   set-for-each            - 遍历集合元素
;;   set-fold                - 折叠集合元素
;;   set-filter              - 过滤元素
;;   set-filter!             - 原地过滤元素
;;   set-remove              - 移除满足条件的元素
;;   set-remove!             - 原地移除满足条件的元素
;;   set-partition           - 拆分为两部分
;;   set-partition!          - 原地拆分


;; 四、集合运算
;; 用于执行集合代数的函数
;;   set-union               - 计算并集
;;   set-intersection        - 计算交集
;;   set-difference          - 计算差集
;;   set-xor                 - 计算对称差
;;   set-union!              - 原地计算并集
;;   set-intersection!       - 原地计算交集
;;   set-difference!         - 原地计算差集
;;   set-xor!                - 原地计算对称差


;; 五、更新函数
;; 用于增删替换元素的函数
;;   set-adjoin              - 加入元素
;;   set-adjoin!             - 原地加入元素
;;   set-replace             - 替换等值元素
;;   set-replace!            - 原地替换等值元素
;;   set-delete              - 删除元素
;;   set-delete!             - 原地删除元素
;;   set-delete-all          - 删除多个元素
;;   set-delete-all!         - 原地删除多个元素
;;   set-search!             - 通过搜索过程原地更新集合
