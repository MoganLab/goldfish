;; (liii njson) 模块函数分类索引
;;
;; njson 提供基于原生 JSON 句柄的高性能 JSON 操作接口。
;; 它适合处理大对象、频繁更新和需要 schema 校验的结构化数据。

;; ==== 常见用法示例 ====
(import (liii njson))

;; 示例1：解析字符串并读取字段
(let-njson ((root (string->njson "{\"name\":\"Goldfish\",\"nums\":[1,2,3]}")))
  (njson-ref root "name")
) ;let-njson

;; 示例2：判断对象是否包含某个键
(let-njson ((root (string->njson "{\"name\":\"Goldfish\",\"nums\":[1,2,3]}")))
  (njson-contains-key? root "nums")
) ;let-njson

;; 示例3：把原生 JSON 句柄转回普通 JSON 结构
(let-njson ((root (string->njson "{\"a\":1}")))
  (njson->json root)
) ;let-njson

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/njson "string->njson"
;;   bin/gf doc liii/njson "njson-ref"

;; ==== 函数分类索引 ====

;; 一、解析与序列化
;; 用于在字符串、文件和 njson 句柄之间转换的函数
;;   string->njson         - 从字符串解析 njson
;;   file->njson           - 从文件解析 njson
;;   njson->string         - 序列化为紧凑字符串
;;   njson-format-string   - 序列化为格式化字符串
;;   njson->file           - 写入文件
;;   json->njson           - 从普通 JSON 结构转换为 njson
;;   njson->json           - 从 njson 转回普通 JSON 结构
;;   let-njson             - 以绑定形式安全管理 njson 句柄

;; 二、类型与大小
;; 用于判断 njson 类型和资源状态的函数
;;   njson?                - 判断对象是否为 njson
;;   njson-null?           - 判断是否为 null
;;   njson-object?         - 判断是否为对象
;;   njson-array?          - 判断是否为数组
;;   njson-string?         - 判断是否为字符串
;;   njson-number?         - 判断是否为数字
;;   njson-integer?        - 判断是否为整数
;;   njson-boolean?        - 判断是否为布尔值
;;   njson-size            - 获取对象或数组大小
;;   njson-empty?          - 判断对象或数组是否为空
;;   njson-free            - 释放原生 JSON 句柄

;; 三、访问与更新
;; 用于读取和修改 njson 内容的函数
;;   njson-ref             - 按路径读取值
;;   njson-set             - 返回设置后的新 njson
;;   njson-append          - 返回追加后的新 njson
;;   njson-set!            - 原地设置值
;;   njson-append!         - 原地追加值
;;   njson-drop            - 返回删除后的新 njson
;;   njson-drop!           - 原地删除值
;;   njson-contains-key?   - 判断对象是否包含某个键
;;   njson-keys            - 获取对象所有键

;; 四、合并、转换与校验
;; 用于合并结构、转换容器和校验 schema 的函数
;;   njson-merge           - 浅层合并多个 njson
;;   njson-merge!          - 原地浅层合并
;;   njson-deep-merge      - 深层合并多个 njson
;;   njson-deep-merge!     - 原地深层合并
;;   njson-object->alist   - 对象转 alist
;;   njson-object->hash-table - 对象转哈希表
;;   njson-array->list     - 数组转列表
;;   njson-array->vector   - 数组转向量
;;   njson-schema-report   - 执行 schema 校验并返回报告
