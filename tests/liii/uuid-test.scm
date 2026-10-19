;; (liii uuid) 模块函数分类索引
;;
;; uuid 当前提供 UUID v4（随机）与 UUID v7（时间有序）生成功能。
;; 适合生成对象标识、请求 ID、会话 ID、数据库主键和去中心化唯一键。


;; ==== 常见用法示例 ====
(import (liii uuid))


;; 示例1：生成一个新的 UUID v4
(uuid4)


;; 示例2：生成一个新的 UUID v7（时间有序）
(uuid7)


;; 示例3：连续生成多个 UUID
(list (uuid7) (uuid7))


;; 示例4：把 UUID 放入关联数据中使用
(cons 'request-id (uuid7))


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/uuid "uuid4"
;;   bin/gf doc liii/uuid "uuid7"


;; ==== 函数分类索引 ====


;; 一、UUID 生成
;; 用于生成唯一标识的函数
;;   uuid4            - 生成一个新的随机 UUID v4 字符串
;;   uuid7            - 生成一个 RFC 9562 时间有序的 UUID v7 字符串
