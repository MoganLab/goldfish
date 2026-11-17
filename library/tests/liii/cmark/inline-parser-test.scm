;; (liii cmark inline parser) 模块函数分类索引
;;
;; cmark-inline-parser 定义最小的内联解析状态对象，
;; 用来记录当前输入串、当前位置以及正在处理的节点。

;; ==== 常见用法示例 ====
(import (liii cmark inline parser))

(define parser (cmark-inline-parser-new))
(cmark-inline-parser-pos parser)
(cmark-inline-parser-content parser)

;; ==== 函数分类索引 ====
;;
;; 一、记录构造
;;   make-cmark-inline-parser - 创建原始记录实例
;;   cmark-inline-parser-new  - 创建已初始化的解析器
;;
;; 二、谓词与访问器
;;   cmark-inline-parser?             - 判断是否为内联解析器
;;   cmark-inline-parser-content      - 获取当前输入字符串
;;   cmark-inline-parser-set-content! - 设置当前输入字符串
;;   cmark-inline-parser-pos          - 获取当前位置
;;   cmark-inline-parser-set-pos!     - 设置当前位置
;;   cmark-inline-parser-node         - 获取当前节点
;;   cmark-inline-parser-set-node!    - 设置当前节点
