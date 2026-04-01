;; (liii base64) 模块函数分类索引
;;
;; base64 提供字符串和字节向量的 Base64 编解码能力。
;; 适合处理二进制载荷、HTTP 文本协议、嵌入式资源和序列化结果。

;; ==== 常见用法示例 ====
(import (liii base64))

;; 示例1：使用统一入口对字符串进行编码
(base64-encode "Goldfish") ; => "R29sZGZpc2g="

;; 示例2：使用统一入口对字符串进行解码
(base64-decode "R29sZGZpc2g=") ; => "Goldfish"

;; 示例3：显式调用字符串编码接口
(string-base64-encode "Hi") ; => "SGk="

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/base64 "base64-encode"
;;   bin/gf doc liii/base64 "base64-decode"

;; ==== 函数分类索引 ====

;; 一、统一入口
;; 用于根据输入类型自动选择编解码逻辑的函数
;;   base64-encode             - 根据输入类型执行 Base64 编码
;;   base64-decode             - 根据输入类型执行 Base64 解码

;; 二、字符串编解码
;; 用于处理字符串形式 Base64 数据的函数
;;   string-base64-encode      - 把字符串编码为 Base64 字符串
;;   string-base64-decode      - 把 Base64 字符串解码为普通字符串

;; 三、字节向量编解码
;; 用于处理字节向量形式二进制数据的函数
;;   bytevector-base64-encode  - 把字节向量编码为 Base64
;;   bytevector-base64-decode  - 把 Base64 内容解码为字节向量
