;; (liii hash-table) 模块函数分类索引
;;
;; hash-table 提供可变哈希表及其常见访问、更新和遍历接口。
;; 适合缓存、索引表、配置项容器和高频键值查找场景。


;; ==== 常见用法示例 ====
(import (liii hash-table))


;; 示例1：创建哈希表并读取已有键
(define ht (hash-table 'a 1 'b 2))
(hash-table-ref/default ht 'a 'missing)


;; 示例2：判断是否包含某个键
(hash-table-contains? ht 'b)


;; 示例3：获取哈希表大小
(hash-table-size ht)


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/hash-table "hash-table"
;;   bin/gf doc liii/hash-table "hash-table-ref/default"


;; ==== 函数分类索引 ====


;; 一、构造函数
;; 用于创建哈希表的函数
;;   make-hash-table          - 创建空哈希表
;;   hash-table               - 从键值对直接构造哈希表
;;   hash-table-unfold        - 通过 unfold 规则构造哈希表
;;   alist->hash-table        - 从 alist 构造哈希表


;; 二、谓词函数
;; 用于判断哈希表状态的函数
;;   hash-table?              - 判断对象是否为哈希表
;;   hash-table-contains?     - 判断是否包含某个键
;;   hash-table-empty?        - 判断哈希表是否为空
;;   hash-table=?             - 判断两个哈希表是否相等
;;   hash-table-mutable?      - 判断哈希表是否可变


;; 三、访问与更新
;; 用于读写哈希表内容的函数
;;   hash-table-ref           - 按键读取值
;;   hash-table-ref/default   - 未命中时返回默认值
;;   hash-table-set!          - 原地设置键值对
;;   hash-table-delete!       - 原地删除键
;;   hash-table-intern!       - 未命中时插入新值
;;   hash-table-update!       - 按过程更新值
;;   hash-table-update!/default - 按过程更新，未命中时使用默认值
;;   hash-table-pop!          - 弹出一个键对应值
;;   hash-table-clear!        - 清空哈希表


;; 四、统计与遍历
;; 用于统计和遍历哈希表的函数
;;   hash-table-size          - 获取元素个数
;;   hash-table-find          - 查找满足条件的项
;;   hash-table-count         - 统计满足条件的项数
;;   hash-table-fold          - 折叠所有键值对
;;   hash-table-for-each      - 遍历所有键值对
;;   hash-table-map->list     - 映射后转为列表


;; 五、转换函数
;; 用于查看或复制哈希表内容的函数
;;   hash-table-keys          - 获取所有键
;;   hash-table-values        - 获取所有值
;;   hash-table-entries       - 获取键值条目
;;   hash-table->alist        - 转为 alist
;;   hash-table-copy          - 复制哈希表
