;; (liii uri) 模块介绍与用法示例
;;
;; (liii uri) 是一个基于 RFC 3986 的 URI 处理库，提供完整的 URI
;; 解析、构造、修改和转换功能。

(import (liii uri))

;; ==== 模块概述 ====
;;
;; 本库将 URI 处理功能分散在多个子模块中：
;;
;; - (liii uri-record)    - 记录类型定义和基础访问器
;; - (liii uri-parse)     - 解析和构建辅助函数
;; - (liii uri-predicate) - 谓词函数（uri-absolute? 等）
;; - (liii uri-compare)   - 比较函数（uri=? 等）
;; - (liii uri-make)      - 构造器（make-uri, string->uri 等）
;; - (liii uri-transform) - 修改函数（uri-with-*, uri-extend-query 等）
;; - (liii uri-convert)   - 转换函数（uri->string, uri->human-string）
;;
;; (liii uri) 作为主模块，统一导出以上所有功能。

;; ==== 如何查看函数的文档和用例 ====
;;
;; 查看主模块文档：
;;   gf doc liii/uri
;;
;; 查看子模块文档：
;;   gf doc liii/uri-record
;;   gf doc liii/uri-parse
;;   gf doc liii/uri-predicate
;;   gf doc liii/uri-compare
;;   gf doc liii/uri-make
;;   gf doc liii/uri-transform
;;   gf doc liii/uri-convert
;;
;; 查看子模块源码：
;;   gf source liii/uri-record
;;   gf source liii/uri-transform
;;
;; 查看具体函数文档：
;;   gf doc "make-uri"
;;   gf doc "uri->string"
;;   gf doc "uri-with-path"

;; ==== 函数分类索引 ====
;;
;; 一、构造 URI
;;   make-uri        - 从各组件构造 URI
;;   uri-build       - 使用关键字参数构造 URI
;;   string->uri     - 从字符串解析 URI
;;
;; 二、访问 URI 组件
;;   uri-scheme      - 获取 scheme（如 http, https）
;;   uri-host        - 获取主机名
;;   uri-port        - 获取端口（含默认端口）
;;   uri-path        - 获取路径
;;   uri-query       - 获取 query 参数（alist 格式）
;;   uri-query-ref   - 获取指定 query 参数值
;;   uri-fragment    - 获取 fragment
;;   uri-user        - 获取用户名
;;   uri-password    - 获取密码
;;
;; 三、修改 URI
;;   uri-with-scheme    - 替换 scheme
;;   uri-with-host      - 替换 host
;;   uri-with-port      - 替换 port
;;   uri-with-path      - 替换 path
;;   uri-with-fragment  - 替换 fragment
;;   uri-extend-query   - 添加 query 参数
;;   uri-without-query  - 移除所有 query 参数
;;   uri-without-query-param - 移除指定 query 参数
;;   uri-join-path      - 在 path 后追加路径段
;;
;; 四、URI 转换
;;   uri->string       - URI 转为字符串
;;   uri->human-string - 生成人类可读的字符串（隐藏敏感信息）
;;
;; 五、URI 比较
;;   uri=?       - URI 相等比较
;;   uri<?       - URI 小于比较
;;   uri>?       - URI 大于比较
;;
;; 六、谓词函数
;;   uri-absolute?      - 是否为绝对 URI
;;   uri-relative?      - 是否为相对 URI
;;   uri-default-port?  - 是否使用默认端口
;;   uri-network-scheme?- 是否为网络协议（http/https/ftp等）
;;
;; 七、编码/解码
;;   uri-encode         - 百分比编码
;;   uri-decode         - 百分比解码
;;   query-string->alist - query 字符串解析为 alist
;;   alist->query-string - alist 转为 query 字符串

;; ==== 常见用法示例 ====

;; 示例1：从字符串解析 URI 并访问组件
(define u1 (string->uri "https://user:pass@api.example.com:8443/v1/users?id=123#profile"))
(uri-scheme u1)    ; => "https"
(uri-host u1)      ; => "api.example.com"
(uri-port u1)      ; => 8443
(uri-path u1)      ; => "/v1/users"
(uri-query-ref u1 "id") ; => "123"
(uri-fragment u1)  ; => "profile"

;; 示例2：构造 API 请求 URI
(define base-uri (string->uri "https://api.example.com/v1"))
(define api-uri
  (uri-extend-query
    (uri-join-path base-uri "users" "123")
    '(("fields" . "name,email") ("include" . "profile"))
  ) ;uri-extend-query
) ;define
(uri->string api-uri)
; => "https://api.example.com/v1/users/123?fields=name%2Cemail&include=profile"

;; 示例3：修改 URI 的 scheme（http 转 https）
(define insecure (string->uri "http://example.com/path"))
(define secure (uri-with-scheme insecure "https"))
(uri->string secure)  ; => "https://example.com/path"

;; 示例4：移除敏感信息，生成日志友好的 URI
(define sensitive (string->uri "https://admin:secret@db.example.com:3306/admin?password=123"))
(uri->human-string sensitive)
; => "https://db.example.com:3306/admin"

;; 示例5：分页 API 请求构造
(define list-api (string->uri "https://api.example.com/items"))
(define page-2
  (uri-extend-query list-api '(("page" . "2") ("limit" . "20")))
) ;define
(uri->string page-2)
; => "https://api.example.com/items?page=2&limit=20"

;; 示例6：合并基础 URI 和相对路径
(define base (string->uri "https://example.com/docs/api/"))
(define ref (string->uri "../guide/intro.md"))
(uri->string (uri-join base ref))
; => "https://example.com/docs/guide/intro.md"

;; 示例7：检查 URI 是否使用默认端口
(uri-default-port? (string->uri "https://example.com/"))       ; => #t
(uri-default-port? (string->uri "https://example.com:8443/"))  ; => #f

;; 示例8：URI 相等比较（忽略默认端口差异）
(uri=? (string->uri "https://example.com/")
       (string->uri "https://example.com:443/")
) ;uri=?
; => #t
