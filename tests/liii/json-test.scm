;; (liii json) 模块函数分类索引
;;
;; json 提供“JSON 值 <-> Scheme 数据”的轻量转换与访问接口。
;; 它更偏向纯 Scheme 数据结构操作，适合配置、小型数据交换和测试输入输出整理。

;; ==== 常见用法示例 ====
(import (liii json))

;; 示例1：把字符串解析为 JSON 数据
(define j (string->json "{\"name\":\"Goldfish\",\"age\":18}"))
(json-ref j "name") ; => "Goldfish"

;; 示例2：判断对象是否包含指定键
(json-contains-key? j "age") ; => #t

;; 示例3：把 JSON 数据重新序列化为字符串
(json->string j) ; => "{\"name\":\"Goldfish\",\"age\":18}"

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/json "string->json"
;;   bin/gf doc liii/json "json-ref"

;; ==== 函数分类索引 ====

;; 一、解析与序列化
;; 用于在字符串和 JSON 数据之间转换的函数
;;   json-string-escape  - 转义 JSON 字符串内容
;;   string->json        - 把 JSON 字符串解析成 Scheme 数据
;;   json->string        - 把 JSON 数据序列化为字符串

;; 二、类型判定
;; 用于判断 JSON 值类型的函数
;;   json-null?          - 判断是否为 JSON null
;;   json-object?        - 判断是否为对象
;;   json-array?         - 判断是否为数组
;;   json-string?        - 判断是否为字符串
;;   json-float?         - 判断是否为浮点数
;;   json-number?        - 判断是否为数字
;;   json-integer?       - 判断是否为整数
;;   json-boolean?       - 判断是否为布尔值

;; 三、访问与更新
;; 用于读取和修改 JSON 数据的函数
;;   json-ref            - 按路径读取值
;;   json-set            - 设置路径上的值
;;   json-push           - 向数组追加元素
;;   json-drop           - 删除路径上的值
;;   json-reduce         - 归约 JSON 结构
;;   json-ref-string     - 按字符串类型读取值
;;   json-ref-number     - 按数字类型读取值
;;   json-ref-integer    - 按整数类型读取值
;;   json-ref-boolean    - 按布尔类型读取值
;;   json-get-or-else    - 当值为 JSON null 时返回默认值

;; 四、对象辅助函数
;; 用于读取 JSON 对象键集合的函数
;;   json-contains-key?  - 判断对象是否包含某个键
;;   json-keys           - 获取对象的所有键
