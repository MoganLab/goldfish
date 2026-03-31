;; (liii range) 模块函数分类索引
;;
;; range 是一种惰性序列（lazy sequence），元素按需计算，内存效率高。
;; 与 (liii list) 的 iota 不同：
;;   - iota 立即生成完整列表，占用 O(n) 内存
;;   - range 惰性求值，占用 O(1) 内存，支持随机访问和切片操作

;; ==== 常见用法示例 ====
(import (liii range))

;; 示例1：循环迭代
(range-for-each
  (lambda (x) (display x) (newline))
  (numeric-range 0 5)                ; 输出: 0 1 2 3 4
) ;range-for-each

;; 示例2：循环求和
;; range-fold 是处理累加的推荐方式，纯函数无副作用
(range-fold + 0 (numeric-range 1 11)) ; => 55

;; 示例3：切片截取
;; 使用 take-right 取末尾元素，体现惰性序列的随机访问能力
(define r (numeric-range 0 100))
(range->list (range-take-right r 3)) ; => (97 98 99)

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/range "range?"
;;   bin/gf doc liii/range "numeric-range"

;; ==== 函数分类索引 ====

;; 一、构造函数
;; 用于创建 range 对象的函数
;;   range            - 使用长度和索引器函数创建 range
;;   numeric-range    - 创建数值范围 range
;;   vector-range     - 从向量创建 range
;;   string-range     - 从字符串创建 range
;;   iota-range       - 创建类似 iota 的 range
;;   range-append     - 连接多个 range

;; 二、谓词函数
;; 用于判断类型和相等性的函数
;;   range?           - 判断是否为 range 对象
;;   range=?          - 判断两个 range 是否相等
;;   range-any        - 判断是否存在满足条件的元素
;;   range-every      - 判断是否所有元素都满足条件

;; 三、属性访问
;; 用于获取 range 属性的函数
;;   range-length     - 获取 range 长度
;;   range-first      - 获取第一个元素
;;   range-last       - 获取最后一个元素

;; 四、元素访问
;; 用于访问 range 中元素的函数
;;   range-ref        - 按索引获取元素
;;   range-count      - 统计满足条件的元素数量

;; 五、子范围操作
;; 用于截取和分割 range 的函数
;;   subrange         - 获取子 range
;;   range-segment    - 将 range 分割成多个段
;;   range-split-at   - 在指定位置分割 range
;;   range-take       - 取前 n 个元素
;;   range-take-right - 取后 n 个元素
;;   range-drop       - 跳过前 n 个元素
;;   range-drop-right - 跳过后 n 个元素
;;   range-reverse    - 反转 range

;; 六、遍历操作
;; 用于遍历 range 元素的函数
;;   range-map->list    - 映射为列表
;;   range-map->vector  - 映射为向量
;;   range-for-each     - 遍历执行副作用
;;   range-fold         - 左折叠
;;   range-fold-right   - 右折叠

;; 七、过滤操作
;; 用于过滤 range 元素的函数
;;   range-filter->list     - 过滤并转为列表
;;   range-filter->vector   - 过滤并转为向量
;;   range-remove->list     - 移除满足条件的元素并转为列表
;;   range-remove->vector   - 移除满足条件的元素并转为向量

;; 八、类型转换
;; 用于将 range 转换为其他数据类型的函数
;;   vector->range      - 向量转为 range
;;   range->list        - 转为列表
;;   range->vector      - 转为向量
;;   range->string      - 转为字符串
;;   range->generator   - 转为生成器
