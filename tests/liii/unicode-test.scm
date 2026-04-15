;; (liii unicode) 模块函数分类索引
;;
;; unicode 提供 UTF-8、UTF-16 与码点之间的转换能力。
;; 适合实现编码转换、协议编解码、字符工具和底层文本处理逻辑。


;; ==== 常见用法示例 ====
(import (liii unicode))


;; 示例1：把码点编码为 UTF-8
(codepoint->utf8 20013)


;; 示例2：在字符串和 UTF-8 之间来回转换
(utf8->string (string->utf8 "你好"))


;; 示例3：把码点转换为十六进制字符串
(codepoint->hexstr 128640)


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/unicode "codepoint->utf8"
;;   bin/gf doc liii/unicode "utf8->utf16be"


;; ==== 函数分类索引 ====


;; 一、UTF-8 函数
;; 用于处理 UTF-8 编码和码点转换的函数
;;   utf8->string              - UTF-8 字节向量转字符串
;;   string->utf8              - 字符串转 UTF-8 字节向量
;;   utf8-string-length        - 获取 UTF-8 字符串长度
;;   u8-substring              - 截取 UTF-8 子串
;;   bytevector-advance-utf8   - 在 UTF-8 字节向量中前进
;;   codepoint->utf8           - 码点转 UTF-8 字节向量
;;   utf8->codepoint           - UTF-8 字节向量转码点


;; 二、UTF-16BE 函数
;; 用于处理大端 UTF-16 的函数
;;   codepoint->utf16be        - 码点转 UTF-16BE
;;   utf16be->codepoint        - UTF-16BE 转码点
;;   utf8->utf16be             - UTF-8 转 UTF-16BE
;;   utf16be->utf8             - UTF-16BE 转 UTF-8
;;   bytevector-utf16be-advance - 在 UTF-16BE 字节向量中前进


;; 三、UTF-16LE 函数
;; 用于处理小端 UTF-16 的函数
;;   codepoint->utf16le        - 码点转 UTF-16LE
;;   utf16le->codepoint        - UTF-16LE 转码点
;;   utf8->utf16le             - UTF-8 转 UTF-16LE
;;   utf16le->utf8             - UTF-16LE 转 UTF-8
;;   bytevector-utf16le-advance - 在 UTF-16LE 字节向量中前进


;; 四、码点与常量
;; 用于码点显示和常量读取的函数
;;   hexstr->codepoint         - 十六进制字符串转码点
;;   codepoint->hexstr         - 码点转十六进制字符串
;;   unicode-max-codepoint     - 最大合法 Unicode 码点
;;   unicode-replacement-char  - Unicode 替换字符常量
