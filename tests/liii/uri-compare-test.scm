;; (liii uri-compare) 模块函数分类索引
;;
;; uri-compare 模块提供 URI 的比较和哈希函数。


(import (liii uri-record)
  (liii uri-compare)
) ;import


;; ==== 常见用法示例 ====


;; 示例1：URI 相等比较
(uri=? (make-uri-raw "https"
         "example.com"
         "/"
         '()
         #f
       ) ;make-uri-raw
  (make-uri-raw "https"
    "example.com"
    "/"
    '()
    #f
  ) ;make-uri-raw
) ;uri=?


;; 示例2：URI 字典序比较
(uri<? (make-uri-raw "http" "a.com" "/" '() #f)
  (make-uri-raw "http" "b.com" "/" '() #f)
) ;uri<?


;; 示例3：URI 哈希值
(uri-hash (make-uri-raw "https"
            "example.com"
            "/path"
            '()
            #f
          ) ;make-uri-raw
) ;uri-hash


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/uri-compare "uri=?"
;;   bin/gf doc liii/uri-compare "uri-hash"


;; ==== 函数分类索引 ====
;;
;; 一、相等比较
;;   uri=?   - 检查两个 URI 是否相等
;;
;; 二、字典序比较
;;   uri<?   - 检查第一个 URI 是否小于第二个
;;   uri>?   - 检查第一个 URI 是否大于第二个
;;
;; 三、哈希函数
;;   uri-hash - 计算 URI 的哈希值
