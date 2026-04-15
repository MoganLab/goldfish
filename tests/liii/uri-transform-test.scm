;; (liii uri-transform) 模块函数分类索引
;;
;; uri-transform 模块提供 URI 的修改和转换函数。


(import (liii uri-record)
  (liii uri-transform)
  (liii uri-convert)
) ;import


;; ==== 常见用法示例 ====


;; 示例1：修改 URI 的 scheme
(uri-with-scheme (make-uri-raw "http"
                   "example.com"
                   "/"
                   '()
                   #f
                 ) ;make-uri-raw
  "https"
) ;uri-with-scheme


;; 示例2：更新查询参数
(uri-extend-query (make-uri-raw "https"
                    "api.com"
                    "/"
                    '()
                    #f
                  ) ;make-uri-raw
  '(("page" . "1") ("limit" . "10"))
) ;uri-extend-query


;; 示例3：URI 转字符串
(uri->string (make-uri-raw "https"
               "example.com"
               "/path"
               '(("a" . "1"))
               "frag"
             ) ;make-uri-raw
) ;uri->string
;; => "https://example.com/path?a=1#frag"


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/uri-transform "uri-with-scheme"
;;   bin/gf doc liii/uri-transform "uri->string"


;; ==== 函数分类索引 ====
;;
;; 一、with- 系列函数（替换组件）
;;   uri-with-scheme    - 替换 scheme
;;   uri-with-host      - 替换 host
;;   uri-with-port      - 替换 port
;;   uri-with-path      - 替换 path
;;   uri-with-fragment  - 替换 fragment
;;
;; 二、query 更新函数
;;   uri-update-query      - 使用函数更新 query
;;   uri-extend-query      - 扩展 query
;;   uri-without-query     - 移除所有 query
;;   uri-without-query-param - 移除指定 query 参数
;;
;; 三、路径操作函数
;;   uri-join-path  - 在 path 后追加段
;;   uri-with-name  - 替换文件名（最后一级）
;;   uri-with-suffix - 替换文件后缀
;;   uri-join       - 合并两个 URI
;;
;; 四、转换函数
;;   uri->string      - URI 转为字符串
;;   uri->human-string - 生成人类可读的字符串（去除敏感信息）
