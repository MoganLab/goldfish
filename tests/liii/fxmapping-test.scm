;; (liii fxmapping) 模块函数分类索引
;;
;; fxmapping 是以 fixnum 为键的有序映射结构，兼具映射查询与区间操作能力。
;; 适合索引表、稠密整数键空间、范围裁剪和基于顺序的映射运算。


;; ==== 常见用法示例 ====
(import (liii fxmapping))


;; 示例1：构造一个 fixnum 键映射并按键读取
(define m
  (fxmapping 0 'zero 1 'one 2 'two)
) ;define
(fxmapping-ref m 1 (lambda () 'missing))


;; 示例2：读取映射大小
(fxmapping-size m)


;; 示例3：判断映射是否为空
(fxmapping-empty? (fxmapping))


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/fxmapping "fxmapping"
;;   bin/gf doc liii/fxmapping "fxmapping-ref"


;; ==== 函数分类索引 ====


;; 一、构造函数
;; 用于创建 fxmapping 的函数
;;   fxmapping                        - 从键值对直接构造映射
;;   fxmapping-unfold                 - 通过 unfold 规则构造映射
;;   fxmapping-accumulate             - 通过累加方式构造映射
;;   alist->fxmapping                 - 从 alist 构造映射
;;   alist->fxmapping/combinator      - 使用合并规则从 alist 构造映射


;; 二、谓词与访问
;; 用于判断状态和按键访问的函数
;;   fxmapping?                       - 判断对象是否为 fxmapping
;;   fxmapping-contains?              - 判断是否包含某个键
;;   fxmapping-empty?                 - 判断映射是否为空
;;   fxmapping-disjoint?              - 判断两个映射是否键域不相交
;;   fxmapping-ref                    - 按键读取值
;;   fxmapping-min                    - 获取最小键对应项
;;   fxmapping-max                    - 获取最大键对应项


;; 三、更新函数
;; 用于增删改键值对的函数
;;   fxmapping-adjoin                 - 加入键值对
;;   fxmapping-adjoin/combinator      - 通过合并规则加入键值对
;;   fxmapping-set                    - 设置键对应值
;;   fxmapping-adjust                 - 根据旧值调整新值
;;   fxmapping-delete                 - 删除一个键
;;   fxmapping-delete-all             - 删除多个键
;;   fxmapping-update                 - 更新一个键的值
;;   fxmapping-alter                  - 按过程修改一个键
;;   fxmapping-delete-min             - 删除最小键
;;   fxmapping-update-min             - 更新最小键
;;   fxmapping-pop-min                - 弹出最小键项
;;   fxmapping-delete-max             - 删除最大键
;;   fxmapping-update-max             - 更新最大键
;;   fxmapping-pop-max                - 弹出最大键项


;; 四、统计与遍历
;; 用于统计、映射和折叠 fxmapping 的函数
;;   fxmapping-size                   - 获取键值对数量
;;   fxmapping-find                   - 查找满足条件的项
;;   fxmapping-count                  - 统计满足条件的项数
;;   fxmapping-any?                   - 判断是否存在满足条件的项
;;   fxmapping-every?                 - 判断是否全部满足条件
;;   fxmapping-map                    - 映射键值对
;;   fxmapping-for-each               - 遍历键值对
;;   fxmapping-fold                   - 左折叠
;;   fxmapping-fold-right             - 右折叠
;;   fxmapping-map->list              - 映射后转为列表
;;   fxmapping-filter                 - 过滤项
;;   fxmapping-remove                 - 移除满足条件的项
;;   fxmapping-partition              - 拆分为两部分


;; 五、转换与视图
;; 用于把 fxmapping 转成其他结构的函数
;;   fxmapping->alist                 - 转为递增 alist
;;   fxmapping->decreasing-alist      - 转为递减 alist
;;   fxmapping-keys                   - 获取所有键
;;   fxmapping-values                 - 获取所有值
;;   fxmapping->generator             - 转为递增生成器
;;   fxmapping->decreasing-generator  - 转为递减生成器


;; 六、比较、区间与集合运算
;; 用于比较映射、做区间裁剪和集合代数的函数
;;   fxmapping=?                      - 判断两个映射是否相等
;;   fxmapping<?                      - 判断是否真子映射
;;   fxmapping>?                      - 判断是否真超映射
;;   fxmapping<=?                     - 判断是否子映射
;;   fxmapping>=?                     - 判断是否超映射
;;   fxmapping-union                  - 计算并集
;;   fxmapping-intersection           - 计算交集
;;   fxmapping-difference             - 计算差集
;;   fxmapping-xor                    - 计算对称差
;;   fxmapping-union/combinator       - 通过合并规则计算并集
;;   fxmapping-intersection/combinator - 通过合并规则计算交集
;;   fxsubmapping=                    - 判断子区间映射是否相等
;;   fxmapping-open-interval          - 取开区间
;;   fxmapping-closed-interval        - 取闭区间
;;   fxmapping-open-closed-interval   - 取左开右闭区间
;;   fxmapping-closed-open-interval   - 取左闭右开区间
;;   fxsubmapping<                    - 判断是否真子区间映射
;;   fxsubmapping<=                   - 判断是否子区间映射
;;   fxsubmapping>                    - 判断是否真超区间映射
;;   fxsubmapping>=                   - 判断是否超区间映射
;;   fxmapping-split                  - 按键拆分映射
;;   fxmapping-relation-map           - 映射关系型结果
