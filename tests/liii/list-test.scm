;; (liii list) 模块函数分类索引
;;
;; list 提供面向 Scheme 列表的常用构造、切片、遍历、搜索与归约操作。
;; 它是函数式数据处理里最常用的一组基础工具。

;; ==== 常见用法示例 ====
(import (liii list))

;; 示例1：从列表头部截取元素
(take '(1 2 3 4 5) 3) ; => (1 2 3)

;; 示例2：把多个列表按位置压成元组列表
(zip '(a b c) '(1 2 3)) ; => ((a 1) (b 2) (c 3))

;; 示例3：把嵌套列表拍平成一维列表
(flatten '((1 2) (3 (4 5)))) ; => (1 2 3 4 5)

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/list "take"
;;   bin/gf doc liii/list "flatten"

;; ==== 函数分类索引 ====

;; 一、构造函数
;; 用于创建和判断列表结构的函数
;;   circular-list     - 创建循环列表
;;   iota              - 生成递增列表
;;   list-copy         - 复制列表
;;   xcons             - 反向参数顺序构造 pair
;;   cons*             - 连续构造嵌套 pair
;;   null-list?        - 判断是否为空列表
;;   circular-list?    - 判断是否为循环列表
;;   proper-list?      - 判断是否为真列表
;;   dotted-list?      - 判断是否为点对尾列表

;; 二、选择与切片
;; 用于按位置访问和截取列表的函数
;;   first             - 获取第 1 个元素
;;   second            - 获取第 2 个元素
;;   third             - 获取第 3 个元素
;;   fourth            - 获取第 4 个元素
;;   fifth             - 获取第 5 个元素
;;   sixth             - 获取第 6 个元素
;;   seventh           - 获取第 7 个元素
;;   eighth            - 获取第 8 个元素
;;   ninth             - 获取第 9 个元素
;;   tenth             - 获取第 10 个元素
;;   take              - 取前 n 个元素
;;   drop              - 跳过前 n 个元素
;;   take-right        - 取后 n 个元素
;;   drop-right        - 跳过后 n 个元素
;;   split-at          - 在指定位置拆分列表
;;   list-take         - 边界宽容版取前 n 个元素
;;   list-drop         - 边界宽容版跳过前 n 个元素
;;   list-take-right   - 边界宽容版取后 n 个元素
;;   list-drop-right   - 边界宽容版跳过后 n 个元素
;;   last-pair         - 获取最后一个 pair
;;   last              - 获取最后一个元素

;; 三、遍历与归约
;; 用于映射、折叠和计数的函数
;;   fold              - 左折叠
;;   fold-right        - 右折叠
;;   reduce            - 左归约
;;   reduce-right      - 右归约
;;   map               - 映射列表
;;   for-each          - 遍历执行副作用
;;   append-map        - 映射后拼接
;;   flat-map          - 映射并扁平化
;;   count             - 统计满足条件的元素个数
;;   any               - 判断是否存在满足条件的元素
;;   every             - 判断是否全部满足条件

;; 四、搜索与过滤
;; 用于查找、筛选和删除元素的函数
;;   find              - 查找第一个满足条件的元素
;;   list-index        - 查找第一个满足条件的索引
;;   take-while        - 从前方连续取满足条件的元素
;;   drop-while        - 从前方连续跳过满足条件的元素
;;   filter            - 保留满足条件的元素
;;   partition         - 分成满足和不满足两部分
;;   remove            - 移除满足条件的元素
;;   delete            - 删除等值元素

;; 五、结构辅助
;; 用于拼装、扁平化和长度判断的函数
;;   zip               - 按位置把多个列表压缩为元组列表
;;   flatten           - 拍平嵌套列表
;;   list-null?        - 判断是否为空列表
;;   list-not-null?    - 判断是否为非空列表
;;   not-null-list?    - 判断是否为非空真列表
;;   length=?          - 判断长度是否等于给定值
;;   length>?          - 判断长度是否大于给定值
;;   length>=?         - 判断长度是否大于等于给定值
