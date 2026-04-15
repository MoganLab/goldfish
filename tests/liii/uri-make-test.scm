;; (liii uri-make) 模块函数分类索引
;;
;; uri-make 模块提供 URI 的构造器函数。


(import (liii uri-record)
  (liii uri-make)
) ;import


;; ==== 常见用法示例 ====


;; 示例1：从字符串构造 URI
(make-uri "https://example.com/path?a=1#frag"
) ;make-uri


;; 示例2：从组件构建 URI
(uri-build :scheme
  "https"
  :host
  "example.com"
  :path
  "/api/v1"
  :query
  '(("key" . "value"))
) ;uri-build


;; 示例3：string->uri 别名
(string->uri "http://test.com/")


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/uri-make "make-uri"
;;   bin/gf doc liii/uri-make "uri-build"


;; ==== 函数分类索引 ====
;;
;; 一、从字符串构造
;;   make-uri      - 从字符串解析构造 URI
;;   string->uri   - make-uri 的别名
;;
;; 二、从组件构建
;;   uri-build     - 使用关键字参数从组件构建 URI
