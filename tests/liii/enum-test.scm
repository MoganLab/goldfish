;; (liii enum) 模块函数分类索引
;;
;; enum 提供枚举类型、枚举值与枚举集合（enum-set）的完整抽象。
;; 适合状态机、有限选项、权限集合和可排序的命名常量。

;; ==== 常见用法示例 ====
(import (liii enum))

;; 示例1：定义一个枚举类型并按名字取值
(define Color (make-enum-type '(red green blue)))
(enum-name (enum-name->enum Color 'green)) ; => green

;; 示例2：读取枚举类型的大小
(enum-type-size Color) ; => 3

;; 示例3：把枚举值放入 enum-set 再做成员判断
(enum-set-contains? (enum-set Color (enum-name->enum Color 'red))
                    (enum-name->enum Color 'red) ; => #t
) ;enum-set-contains?

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/enum "make-enum-type"
;;   bin/gf doc liii/enum "enum-set"

;; ==== 函数分类索引 ====

;; 一、枚举类型与枚举值
;; 用于构造枚举类型并访问枚举值的函数
;;   make-enum-type      - 创建一个枚举类型
;;   enum-type?          - 判断对象是否为枚举类型
;;   enum?               - 判断对象是否为枚举值
;;   enum-type-contains? - 判断枚举类型是否包含某个值
;;   enum-type           - 获取枚举值所属的枚举类型
;;   enum-name           - 获取枚举值的名字
;;   enum-ordinal        - 获取枚举值的序号
;;   enum-value          - 获取枚举值的底层值
;;   enum-name->enum     - 通过名字查找枚举值
;;   enum-ordinal->enum  - 通过序号查找枚举值
;;   enum-name->ordinal  - 通过名字查找序号
;;   enum-name->value    - 通过名字查找底层值
;;   enum-ordinal->name  - 通过序号查找名字
;;   enum-ordinal->value - 通过序号查找底层值

;; 二、枚举类型访问与比较
;; 用于读取枚举类型信息和比较枚举值的函数
;;   enum-type-size      - 获取枚举类型大小
;;   enum-min            - 获取最小枚举值
;;   enum-max            - 获取最大枚举值
;;   enum-type-enums     - 获取所有枚举值
;;   enum-type-names     - 获取所有名字
;;   enum-type-values    - 获取所有底层值
;;   enum=?              - 判断两个枚举值是否相等
;;   enum<?              - 判断两个枚举值是否小于
;;   enum>?              - 判断两个枚举值是否大于
;;   enum<=?             - 判断两个枚举值是否小于等于
;;   enum>=?             - 判断两个枚举值是否大于等于
;;   enum-next           - 获取下一个枚举值
;;   enum-prev           - 获取上一个枚举值
;;   make-enum-comparator - 创建枚举比较器

;; 三、枚举集合构造
;; 用于创建 enum-set 及其辅助构造器的函数
;;   enum-empty-set      - 创建空的枚举集合
;;   enum-type->enum-set - 根据枚举类型创建全集
;;   enum-set            - 直接构造枚举集合
;;   list->enum-set      - 从列表构造枚举集合
;;   make-enumeration    - 创建枚举集合构造器
;;   enum-set-universe   - 获取 enum-set 的全集
;;   enum-set-constructor - 获取 enum-set 构造过程
;;   enum-set-indexer    - 获取 enum-set 索引过程

;; 四、枚举集合操作
;; 用于判断、遍历和组合 enum-set 的函数
;;   enum-set?           - 判断对象是否为枚举集合
;;   enum-set-contains?  - 判断 enum-set 是否包含枚举值
;;   enum-set-member?    - 判断枚举值是否为集合成员
;;   enum-set-empty?     - 判断 enum-set 是否为空
;;   enum-set-disjoint?  - 判断两个 enum-set 是否不相交
;;   enum-set=?          - 判断两个 enum-set 是否相等
;;   enum-set<?          - 判断一个 enum-set 是否真子集于另一个
;;   enum-set>?          - 判断一个 enum-set 是否真超集于另一个
;;   enum-set<=?         - 判断一个 enum-set 是否为另一个的子集
;;   enum-set>=?         - 判断一个 enum-set 是否为另一个的超集
;;   enum-set-subset?    - 判断是否为子集
;;   enum-set-any?       - 判断是否存在满足条件的成员
;;   enum-set-every?     - 判断是否所有成员都满足条件
;;   enum-set-type       - 获取 enum-set 的枚举类型
;;   enum-set-size       - 获取元素个数
;;   enum-set->enum-list - 转为枚举值列表
;;   enum-set->list      - 转为普通列表
;;   enum-set-map->list  - 映射后转为列表
;;   enum-set-count      - 统计满足条件的成员个数
;;   enum-set-filter     - 过滤成员
;;   enum-set-filter!    - 原地过滤成员
;;   enum-set-remove     - 移除满足条件的成员
;;   enum-set-remove!    - 原地移除满足条件的成员
;;   enum-set-for-each   - 遍历成员
;;   enum-set-fold       - 折叠成员
;;   enum-set-adjoin     - 加入成员
;;   enum-set-adjoin!    - 原地加入成员
;;   enum-set-delete     - 删除成员
;;   enum-set-delete!    - 原地删除成员
;;   enum-set-delete-all - 删除多个成员
;;   enum-set-delete-all! - 原地删除多个成员
;;   enum-set-union      - 计算并集
;;   enum-set-union!     - 原地计算并集
;;   enum-set-intersection - 计算交集
;;   enum-set-intersection! - 原地计算交集
;;   enum-set-difference - 计算差集
;;   enum-set-difference! - 原地计算差集
;;   enum-set-xor        - 计算对称差
;;   enum-set-xor!       - 原地计算对称差
;;   enum-set-complement - 计算补集
;;   enum-set-complement! - 原地计算补集
