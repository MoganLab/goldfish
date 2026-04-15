;; (liii iset) 模块函数分类索引
;;
;; iset 是面向整数区间优化的集合结构，适合稠密整数、区间裁剪和范围合并。
;; 与通用 set 相比，它更强调整数域上的区间和子集运算。


;; ==== 常见用法示例 ====
(import (liii iset))


;; 示例1：从整数列表构造一个 iset
(define s (list->iset '(1 2 3 5 8)))
(iset-size s)


;; 示例2：判断某个整数是否为成员
(iset-contains? s 5)


;; 示例3：直接创建一个整数区间集合
(make-range-iset 10 15)


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/iset "iset"
;;   bin/gf doc liii/iset "make-range-iset"


;; ==== 函数分类索引 ====


;; 一、构造函数
;; 用于创建和复制整数集合的函数
;;   iset                 - 从多个整数直接构造 iset
;;   iset-unfold          - 通过 unfold 规则构造 iset
;;   make-range-iset      - 按整数区间构造 iset
;;   iset-copy            - 复制一个 iset
;;   iset->list           - 转为整数列表
;;   list->iset           - 从列表构造 iset
;;   list->iset!          - 原地式从列表构造 iset


;; 二、谓词与访问
;; 用于判断状态和读取边界元素的函数
;;   iset?                - 判断对象是否为 iset
;;   iset-contains?       - 判断是否包含某个整数
;;   iset-empty?          - 判断 iset 是否为空
;;   iset-disjoint?       - 判断两个 iset 是否不相交
;;   iset-member          - 查找成员值
;;   iset-min             - 获取最小值
;;   iset-max             - 获取最大值


;; 三、更新函数
;; 用于增删成员和边界元素的函数
;;   iset-adjoin          - 加入整数
;;   iset-adjoin!         - 原地加入整数
;;   iset-delete          - 删除整数
;;   iset-delete!         - 原地删除整数
;;   iset-delete-all      - 删除多个整数
;;   iset-delete-all!     - 原地删除多个整数
;;   iset-search          - 搜索并返回新集合
;;   iset-search!         - 原地搜索并更新
;;   iset-delete-min      - 删除最小值
;;   iset-delete-min!     - 原地删除最小值
;;   iset-delete-max      - 删除最大值
;;   iset-delete-max!     - 原地删除最大值


;; 四、统计与遍历
;; 用于统计、映射和过滤整数集合的函数
;;   iset-size            - 获取元素个数
;;   iset-find            - 查找满足条件的值
;;   iset-count           - 统计满足条件的值个数
;;   iset-any?            - 判断是否存在满足条件的值
;;   iset-every?          - 判断是否全部满足条件
;;   iset-map             - 映射为其他序列
;;   iset-for-each        - 遍历所有值
;;   iset-fold            - 左折叠
;;   iset-fold-right      - 右折叠
;;   iset-filter          - 过滤成员
;;   iset-filter!         - 原地过滤成员
;;   iset-remove          - 移除满足条件的成员
;;   iset-remove!         - 原地移除满足条件的成员
;;   iset-partition       - 拆分为两部分
;;   iset-partition!      - 原地拆分


;; 五、集合与区间运算
;; 用于处理整数区间和集合代数的函数
;;   iset=?               - 判断两个 iset 是否相等
;;   iset<?               - 判断一个 iset 是否真子集于另一个
;;   iset>?               - 判断一个 iset 是否真超集于另一个
;;   iset<=?              - 判断一个 iset 是否为另一个的子集
;;   iset>=?              - 判断一个 iset 是否为另一个的超集
;;   iset-union           - 计算并集
;;   iset-union!          - 原地计算并集
;;   iset-intersection    - 计算交集
;;   iset-intersection!   - 原地计算交集
;;   iset-difference      - 计算差集
;;   iset-difference!     - 原地计算差集
;;   iset-xor             - 计算对称差
;;   iset-xor!            - 原地计算对称差
;;   iset-open-interval   - 取开区间
;;   iset-closed-interval - 取闭区间
;;   iset-open-closed-interval - 取左开右闭区间
;;   iset-closed-open-interval - 取左闭右开区间
;;   isubset=             - 判断区间子集是否相等
;;   isubset<             - 判断是否真子区间集
;;   isubset<=            - 判断是否子区间集
;;   isubset>             - 判断是否真超区间集
;;   isubset>=            - 判断是否超区间集
