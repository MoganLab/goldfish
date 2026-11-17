;; (liii cmark node) 模块函数分类索引
;;
;; cmark-node 是整个 Markdown AST 的底层数据结构模块，
;; 负责提供节点记录、类型集合、节点构造器以及父子关系操作。

;; ==== 常见用法示例 ====
(import (liii cmark node))

(define heading (make-cmark-heading 2))
(cmark-node-type heading)

(define para (make-cmark-paragraph))
(cmark-node-append-child! para (make-cmark-text "Hello"))
(cmark-node-type (cmark-node-first-child para))

;; ==== 函数分类索引 ====
;;
;; 一、基础记录与类型集合
;;   make-cmark-node    - 创建通用 AST 节点
;;   cmark-node?        - 判断是否为 cmark 节点
;;   cmark-node-type    - 获取节点类型
;;   cmark-node-set-type! - 设置节点类型
;;   cmark-node-data    - 获取节点附加数据
;;   cmark-node-set-data! - 设置节点附加数据
;;   cmark-node-children - 获取子节点列表
;;   cmark-node-set-children! - 设置子节点列表
;;   cmark-node-parent  - 获取父节点
;;   cmark-node-set-parent! - 设置父节点
;;   cmark-block-type?  - 判断块级节点类型
;;   cmark-inline-type? - 判断行内节点类型
;;   cmark-all-types    - 获取全部节点类型列表
;;
;; 二、块级节点构造器
;;   make-cmark-document
;;   make-cmark-block-quote
;;   make-cmark-paragraph
;;   make-cmark-heading
;;   make-cmark-code-block
;;   make-cmark-thematic-break
;;   make-cmark-list
;;   make-cmark-item
;;   make-cmark-html-block
;;   make-cmark-math-block
;;
;; 三、行内节点构造器
;;   make-cmark-text
;;   make-cmark-softbreak
;;   make-cmark-linebreak
;;   make-cmark-code
;;   make-cmark-emph
;;   make-cmark-strong
;;   make-cmark-link
;;   make-cmark-image
;;   make-cmark-html-inline
;;   make-cmark-math-inline
;;
;; 四、树结构操作
;;   cmark-node-append-child! - 为父节点追加子节点
;;   cmark-node-first-child   - 获取第一个子节点
;;   cmark-node-last-child    - 获取最后一个子节点
;;
;; 五、常用节点谓词
;;   cmark-node-type?
;;   cmark-document?
;;   cmark-paragraph?
;;   cmark-text?
;;   cmark-heading?
;;   cmark-list?
