;; (liii either) 模块函数分类索引
;;
;; either 用于表达“成功值或错误值”这类二选一结果。
;; 其中 Right 通常表示成功，Left 通常表示失败或提前返回的信息。

;; ==== 常见用法示例 ====
(import (liii either))

;; 示例1：构造一个 Right 值并映射结果
(define ok (from-right 21))
(to-right (either-map (lambda (x) (* x 2)) ok)) ; => 42

;; 示例2：遇到 Left 时回退到默认值
(either-get-or-else (from-left "network-error") 0) ; => 0

;; 示例3：把普通值包装成 Left 或 Right
(to-left (from-left "bad-request")) ; => "bad-request"

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/either "either-map"
;;   bin/gf doc liii/either "either-get-or-else"

;; ==== 函数分类索引 ====

;; 一、构造函数
;; 用于创建和提取 Either 值的函数
;;   from-left          - 创建 Left 值
;;   to-left            - 提取 Left 中的值
;;   from-right         - 创建 Right 值
;;   to-right           - 提取 Right 中的值

;; 二、谓词函数
;; 用于判断 Either 状态的函数
;;   either?            - 判断对象是否为 Either
;;   either-left?       - 判断对象是否为 Left
;;   either-right?      - 判断对象是否为 Right

;; 三、高阶操作
;; 用于映射、遍历和过滤 Either 的函数
;;   either-map         - 仅对 Right 中的值进行映射
;;   either-for-each    - 仅对 Right 中的值执行副作用
;;   either-filter-or-else - 不满足条件时回退到 Left
;;   either-contains?   - 判断 Right 中的值是否满足目标
;;   either-every       - 判断 Right 中的值是否满足谓词
;;   either-any         - 判断 Right 中的值是否存在真值

;; 四、回退函数
;; 用于在 Left 情况下提供默认值或替代结果的函数
;;   either-get-or-else - Left 时返回默认值
;;   either-or-else     - Left 时返回备用 Either
