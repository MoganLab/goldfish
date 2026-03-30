(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;;; ============================================================
;;; (liii fxmapping) - 整数映射（fixnum mapping）
;;; ============================================================
;;;
;;; (liii fxmapping) 是一个 R7RS 库，提供了不可变的整数键映射数据结构，
;;; 类似于其他语言中的 IntMap 或 SortedMap。
;;;
;;; 该模块的函数被组织为以下几个类别：
;;;
;;; --------------------------------------------------------
;;; 1. Constructors（构造器）
;;; --------------------------------------------------------
;;; - fxmapping                          : 从键值对创建映射
;;; - alist->fxmapping                   : 从关联列表创建
;;; - alist->fxmapping/combinator        : 从关联列表创建，带合并函数
;;; - fxmapping-unfold                   : 使用 unfold 模式创建
;;;
;;; --------------------------------------------------------
;;; 2. Predicates（谓词）
;;; --------------------------------------------------------
;;; - fxmapping?                         : 是否为 fxmapping
;;; - fxmapping-contains?                : 是否包含指定键
;;; - fxmapping-empty?                   : 是否为空
;;; - fxmapping-disjoint?                : 两个映射是否不相交
;;;
;;; --------------------------------------------------------
;;; 3. Accessors（访问器）
;;; --------------------------------------------------------
;;; - fxmapping-ref                      : 获取指定键的值
;;; - fxmapping-min                      : 获取键最小的键值对
;;; - fxmapping-max                      : 获取键最大的键值对
;;;
;;; --------------------------------------------------------
;;; 4. Updaters（更新器）
;;; --------------------------------------------------------
;;; - fxmapping-adjoin                   : 添加键值对（不覆盖）
;;; - fxmapping-adjoin/combinator        : 添加键值对，带合并函数
;;; - fxmapping-set                      : 设置键值对（覆盖）
;;; - fxmapping-adjust                   : 调整指定键的值
;;; - fxmapping-delete                   : 删除指定键
;;; - fxmapping-delete-all               : 删除多个键
;;; - fxmapping-delete-min               : 删除最小键
;;; - fxmapping-delete-max               : 删除最大键
;;; - fxmapping-pop-min                  : 弹出最小键
;;; - fxmapping-pop-max                  : 弹出最大键
;;;
;;; --------------------------------------------------------
;;; 5. The whole fxmapping（整体操作）
;;; --------------------------------------------------------
;;; - fxmapping-size                     : 获取大小
;;; - fxmapping-find                     : 查找满足谓词的键值对
;;; - fxmapping-count                    : 统计满足谓词的键值对数量
;;; - fxmapping-any?                     : 是否存在满足谓词的键值对
;;; - fxmapping-every?                   : 是否所有键值对都满足谓词
;;;
;;; --------------------------------------------------------
;;; 6. Mapping and folding（映射和折叠）
;;; --------------------------------------------------------
;;; - fxmapping-map                      : 映射所有值
;;; - fxmapping-for-each                 : 遍历所有键值对
;;; - fxmapping-fold                     : 左折叠
;;; - fxmapping-fold-right               : 右折叠
;;; - fxmapping-map->list                : 映射并转换为列表
;;; - fxmapping-filter                   : 过滤键值对
;;; - fxmapping-remove                   : 移除键值对
;;; - fxmapping-partition                : 按谓词分割映射
;;;
;;; --------------------------------------------------------
;;; 7. Conversion（转换）
;;; --------------------------------------------------------
;;; - fxmapping->alist                   : 转换为关联列表（升序）
;;; - fxmapping->decreasing-alist        : 转换为关联列表（降序）
;;; - fxmapping-keys                     : 获取所有键
;;; - fxmapping-values                   : 获取所有值
;;;
;;; --------------------------------------------------------
;;; 8. Comparison（比较）
;;; --------------------------------------------------------
;;; - fxmapping=?                        : 比较映射是否相等
;;; - fxmapping<?                        : 是否为真子集
;;; - fxmapping>?                        : 是否为真超集
;;; - fxmapping<=?                       : 是否为子集
;;; - fxmapping>=?                       : 是否为超集
;;;
;;; --------------------------------------------------------
;;; 9. Set theory operations（集合操作）
;;; --------------------------------------------------------
;;; - fxmapping-union                    : 并集
;;; - fxmapping-union/combinator         : 并集，带合并函数
;;; - fxmapping-intersection             : 交集
;;; - fxmapping-intersection/combinator  : 交集，带合并函数
;;; - fxmapping-difference               : 差集
;;; - fxmapping-xor                      : 对称差集
;;;
;;; --------------------------------------------------------
;;; 10. Subsets（子集）
;;; --------------------------------------------------------
;;; - fxsubmapping=                      : 获取指定键的子映射
;;; - fxmapping-open-interval            : 开区间 (low, high)
;;; - fxmapping-closed-interval          : 闭区间 [low, high]
;;; - fxmapping-open-closed-interval     : 半开区间 (low, high]
;;; - fxmapping-closed-open-interval     : 半闭区间 [low, high)
;;; - fxsubmapping<                      : 键小于指定值
;;; - fxsubmapping<=                     : 键小于等于指定值
;;; - fxsubmapping>                      : 键大于指定值
;;; - fxsubmapping>=                     : 键大于等于指定值
;;; - fxmapping-split                    : 按指定键分割

;; 这里可以添加一些基础集成测试

(check-report)
