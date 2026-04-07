;; (liii uri-predicate) 模块函数分类索引
;;
;; uri-predicate 模块提供 URI 相关的谓词函数。

(import (liii uri-record)
        (liii uri-predicate)
) ;import

;; ==== 常见用法示例 ====

;; 示例1：检查 URI 是否为绝对 URI
(uri-absolute? (make-uri-raw "https" "example.com" "/" '() #f))  ; => #t
(uri-absolute? (make-uri-raw #f "" "/path" '() #f))             ; => #f

;; 示例2：获取 scheme 的默认端口
(uri-default-port "https")  ; => 443
(uri-default-port "http")   ; => 80

;; 示例3：检查是否为网络 scheme
(uri-network-scheme? "https")  ; => #t
(uri-network-scheme? "ftp")    ; => #t
(uri-network-scheme? "xyz")    ; => #f

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/uri-predicate "uri-absolute?"
;;   bin/gf doc liii/uri-predicate "uri-default-port"

;; ==== 函数分类索引 ====
;;
;; 一、URI 类型谓词
;;   uri-absolute?     - 检查是否为绝对 URI（有 scheme）
;;   uri-relative?     - 检查是否为相对 URI（无 scheme）
;;
;; 二、端口相关
;;   uri-default-port  - 获取 scheme 的默认端口
;;   uri-default-port? - 检查 URI 是否使用默认端口
;;
;; 三、Scheme 检查
;;   uri-network-scheme? - 检查是否为网络 scheme
