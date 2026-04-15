;; (liii option) 模块函数分类索引
;;
;; option 用于表示“有值”或“无值”的可选结果。
;; 它适合安全链式处理、默认值回退，以及避免直接使用 `#f` 表示缺失。


;; ==== 常见用法示例 ====
(import (liii option))


;; 示例1：创建一个带值的 option 并取值
(option-get (option 42))


;; 示例2：在空 option 上提供默认值
(option-get-or-else 0 (none))


;; 示例3：把 option 内的值映射成新结果
(option-map (lambda (x) (* x 2))
  (option 21)
) ;option-map


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/option "option"
;;   bin/gf doc liii/option "option-map"


;; ==== 函数分类索引 ====


;; 一、构造函数
;; 用于创建 option 值的函数
;;   none               - 创建空 option
;;   option             - 创建带值 option


;; 二、谓词函数
;; 用于判断 option 状态的函数
;;   option?            - 判断对象是否为 option
;;   option-defined?    - 判断 option 是否有值
;;   option-empty?      - 判断 option 是否为空


;; 三、高阶操作
;; 用于映射、过滤和遍历 option 的函数
;;   option-map         - 对有值 option 做映射
;;   option-filter      - 按谓词保留或清空 option
;;   option-flat-map    - 返回新的 option 结果
;;   option-for-each    - 对有值 option 执行副作用
;;   option-every       - 判断值是否满足谓词
;;   option-any         - 判断值是否存在真值


;; 四、取值与回退
;; 用于从 option 中读取值的函数
;;   option-get         - 获取内部值，空 option 时报错
;;   option-get-or-else - 空 option 时返回默认值
;;   option-or-else     - 空 option 时返回备用 option


;; 五、比较函数
;; 用于比较两个 option 是否相等的函数
;;   option=?           - 判断两个 option 的状态和值是否相等
