;; (liii json schema data) 模块介绍与用法示例
;;
;; (liii json schema data) 封装 JSON 数据的底层访问约定：对象以键值对
;; alist 表示，数组以向量表示，布尔值兼容 #t/#f 与 true/false 两种写法。

(import (liii json schema data))

;; ==== 模块概述 ====
;;
;; - json-boolean?   - 是否为 JSON 布尔值
;; - json-true?      - 是否为 JSON 布尔真值
;; - json-false?     - 是否为 JSON 布尔假值
;; - key->string     - 键转字符串
;; - entries         - 对象的键值对序列
;; - object-cell     - 查找键值对
;; - object-ref      - 读取键对应的值
;; - object-has?     - 判断键是否存在
;; - option-ref      - 读取选项表中的值
;; - listify         - 值规约为列表
;; - absent          - 缺失值标记
;; - collect-for-each - 遍历并收集结果

;; ==== 如何查看函数的文档和用例 ====
;;
;; 查看模块文档：
;;   gf doc liii/json/schema/data
;;
;; 查看具体函数文档：
;;   gf doc "object-ref"
;;   gf doc "listify"

;; ==== 常见用法示例 ====

;; 示例1：读取对象字段

(define person '(("name" . "Liii") (age . 18)))
(object-ref person "age" 0)
(object-has? person 'name)

;; 示例2：字符串键与符号键等价
(object-ref person 'name #f)

;; 示例3：遍历数组
(listify #(1 2 3))
