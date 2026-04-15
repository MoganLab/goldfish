;; (liii chez) 模块函数索引
;;
;; (liii chez) 提供 Chez Scheme 兼容性函数，用于与 Chez Scheme 的代码保持兼容。

;; ==== 常见用法示例 ====
(import (liii chez))

;; 示例1：判断原子类型
(atom? 'symbol)
(atom? '(a b c))

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/chez "atom?"

;; ==== 函数分类索引 ====

;; 一、类型判断
;; 用于判断数据类型的谓词函数
;;   atom?             - 判断是否为原子（非点对类型）
