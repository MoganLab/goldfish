;; (liii trie) 模块函数分类索引
;;
;; trie 是一类按键路径逐层索引的树形结构，适合前缀匹配和分层键查找。
;; 它常用于词典树、命令补全和分段路径映射。

;; ==== 常见用法示例 ====
(import (liii trie))

;; 示例1：创建空 trie
(define t (make-trie))

;; 示例2：按路径插入值
(trie-insert! t '(g o l d) 'fish)

;; 示例3：按路径读取值并导出为列表
(trie-ref t '(g o l d))
(trie->list t)

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/trie "make-trie"
;;   bin/gf doc liii/trie "trie-insert!"

;; ==== 函数分类索引 ====

;; 一、构造与判定
;; 用于创建和判断 trie 的函数
;;   make-trie        - 创建空 trie
;;   trie?            - 判断对象是否为 trie

;; 二、插入与查找
;; 用于按路径写入和读取 trie 的函数
;;   trie-insert!     - 原地插入路径和值
;;   trie-ref         - 按完整路径读取值
;;   trie-ref*        - 按路径读取子 trie 或值
;;   trie-value       - 读取当前节点保存的值

;; 三、转换函数
;; 用于把 trie 导出为普通结构的函数
;;   trie->list       - 将 trie 转换成列表表示
